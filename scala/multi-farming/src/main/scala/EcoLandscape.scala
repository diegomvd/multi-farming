import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph
import scala.math.pow

object EcoLandscape{

  /**
  * @param r is the radius of the biophysical landscape
  * @param ecr is the ecological connectivity range
  * @return a biophysical composition with every unit in a natural state
  */
  def buildComposition(r: Int,
                       ecr: Int) = Graph[EcoUnit, Long] {
    val sc: SparkContext
    val units: RDD[(VertexId, String)] =
      sc.parallelize( ModCo.apply(r).map{ (_.toLong,EcoUnit("Natural")) }.toSeq )
    val edges: RDD[Edge[Long]] =
      sc.parallelize( ModCo.apply(r).toSet.subsets(2).collect{
        case (pos1,pos2) if ModCo.neighbors(pos1,r,ecr).exists(_ == pos2) =>
          Edge(pos1.toLong,pos2.toLong,0L)
        }
      )
    Graph(units,edges)
  }

  /**
  * Initialization departs from a pristine landscape, thus ecosystem services
  * flow is maximum and equal to 1 in every ecological unit in the landscape.
  * @param r is the radius of the biophysical landscape
  * @return the map of ecosystem service provision in a pristine landscape
  */
  def buildESFlow(r: Int) = VertexRDD[Double] {
    sc.parallelize( ModCo.apply(r).map{ (_.toLong,1.0) }.toSeq )
  }

  /**
  * @param vids are the vertexId of the units to update
  * @param cover is the new land cover
  * @param comp is the biophysical landscape composition
  * @return the updated composition
  */
  def updatedComposition(vids: VertexRDD[VertexId],
                         cover: String
                         comp: Graph[EcoUnit, Long]) = Graph[EcoUnit, Long] {
    val upd_vertices: VertexRDD = comp.vertices.mapValues{ case (vid, _) =>
      if vids.contains(vid) => EcoUnit(cover) } // note that the new cover is the same for every unit
    Graph( upd_vertices, comp.edges )
  }

  /**
  * @param uid is the vertexId of the unit
  * @param cover is the new land cover
  * @param comp is the biophysical landscape composition
  * @return the updated composition
  */
  def updatedComposition(uid: VertexId,
                         cover: String
                         comp: Graph[EcoUnit, Long]) = Graph[EcoUnit, Long] {
    val upd_vertices: VertexRDD = comp.vertices.mapValues{ case (vid, _) =>
      if (uid == vids) => EcoUnit(cover) } // note that the new cover is the same for every unit
    Graph( upd_vertices, comp.edges )
  }

  /**
  * @param comp biophysical landscape's composition
  * @return the vertices in the connected components graph
  */
  def naturalConnectedComponents(comp: Graph[EcoUnit, Long]) = VertexRDD[VertexId]{
    val natural = comp.subgraph(vpred = (vid,eu) => eu.cover == "Natural")
    val ncc = natural.connectedComponents().vertices
  }

  /**
  * @param ncc is the natural connected components, VertexRDD is over the EcoUnits
  * and VertexId refers to the component id
  * @return a map with the number of units in each component
  */
  def nccArea(ncc: VertexRDD[VertexId]) = Map[(VertexId,VertexId), Long] {
    ncc.countByValue()
  }

  /**
  * @param total_area is the total number of EcoUnits in the landscape
  * @return a composition graph joined with the area of the natural fragment they belong to
  */
  def joinNCCArea(comp: Graph[EcoUnit, Long],
                  total_area: Double): Graph[(EcoUnit,Double), Long] = {
    val ncc = naturalConnectedComponents(comp)
    val area = nccArea(ncc)
    val v_area: VertexRDD[Double] = ncc.map{ area.get((_,_)).toDouble }

    comp.outerJoinVertices(v_area){ (id, eu, v_area_opt) =>
      v_area_opt match {
        case Some(v_area) => (eu, v_area/total_area)
        case None => (eu,0.0)
      }
    }
  }

  /**
  * @param comp is the biophysical composition of the landscape
  * @param z is the ES-area scaling exponent
  * @return an RDD with each unit and the ES flow they receive
  */
  def updatedESFlow(comp: Graph[EcoUnit,Long],
                    total_area: Double,
                    z: Double): VertexRDD[Double] = {

    val joined_comp: Graph[(EcoUnit,Double), Long] = joinNCCArea(comp,total_area)

    joined_comp.aggregateMessages[(Int,Double)](
      triplet => {
        if (triplet.srcAttr._1.cover == "Natural") {
          // this assumes that the second attribute is the component's area
          triplet.sendToDst((1,pow(triplet.srcAttr._2.toDouble, z)))
        }
        else triplet.sendToDst((1,0.0))
      },
      (a,b) => (a._1 + b._1, a._2 + b._2)
    ).mapValues( (id,val) => val._2/val._1 )
  }

  /**
  * @param comp is the biophysical composition of the landscape
  * @param es is the ecosystem service flow in each EcoUnit
  * @return a graph joining the ecounits with the ES flow they receive
  */
  def joinES(comp: Graph[EcoUnit,Long],
             es: VertexRDD[Double]): Graph[EcoUnit,Long] = {
     comp.outerJoinVertices(es){ (id, eu, es_opt) =>
       es_opt match {
         case Some(es) => (eu, es)
         case None => (eu, 0.0)
       }
     }
  }

  /**
  * @return a vertexRDD with the propensity for a certain transition in each EcoUnit of the graph
  */
  def propensities(comp: Graph[EcoUnit,Long],
                   es: VertexRDD[Double],
                   s: Double,
                   c: String,
                   f: (Double,Double,Bool) => Double) = VertexRDD[Double] {

    val joined_comp: Graph[(EcoUnit,Double),Long] = joinES(comp,es)
    joined_comp.vertices.mapValues{ (id, (eui, esi)) => EcoUnit.propensity(esi, s, eui.matchCover(c), f) }
  }

  @annotation.tailrec
  def initialize(eco: Graph[EcoUnit,Long],
                 plan: Graph[PlanningUnit,Long],
                 mng: Graph[ManagementUnit,Long],
                 z: Double,
                 fagr: Double,
                 fdeg: Double): Graph[EcoUnit,Long] = {

    val total_area: Int = eco.vertices.count().toInt
    val n_agr: Int = total_area*fagr.toInt
    val n_deg: Int = total_area*fdeg.toInt

    def rec(eco: Graph[EcoUnit,Long],
            plan: Graph[PlanningUnit,Long],
            mng: Graph[ManagementUnit,Long],
            total_area: Int,
            z: Double,
            n_agr: Int,
            n_deg: Int): Graph[EcoUnit,Long] = {
      val n: Int = n_agr + n_deg
      if (n > 0) {
        val rnd_n: Int = rnd.nextInt(n)
        if (rnd_n < n_agr){ // this means that agricultural conversion is chosen
          // first choose a management unit among the available ones
          val mngp: VertexRDD[Double] =
            ManagementLandscape.conversionPropensity(mng,plan,eco,1.0)
          val sum: Double = mngp.reduce(_+_)
          val rnd_x: Double = rnd.nextDouble(sum)
          val mid: VertexId = S3Utils.selectVId(rnd_x,prop)
          // now choose an available planning unit belonging to the management unit, this part is wrong
          // need to go again with the functions
          val plnp: VertexRDD[Double] =
            mng.lookup(mid).conversionPropensity(plan,eco,1.0)
          val pid: VertexId = S3Utils.selectVId(rnd_x,prop)
          val vids: VertexRDD[VertexId] = plan.lookup(pid)
          mng.lookup(mid).stg match{
            case "Sharing" => val upd_eco: Graph[EcoUnit,Long] = updatedComposition(vids, "Low-intensity", eco)
            case "Sparing" => val upd_eco: Graph[EcoUnit,Long] = updatedComposition(vids, "High-intensity", eco)
            case other => println(s"Trying to use strategy $other, only Sharing and Sparing are allowed")
          }
          // update remaining number of land cover changes
          val upd_n_deg = n_deg
          if n_agr>0 { val upd_n_agr = n_agr - 1 }
          else {val upd_n_agr = n_agr}
        }
        else{ // this means that a degrading transition is chosen
          // calculate ecosystem service flow and the degradation propensity
          val es: VertexRDD[Double] = updatedESFlow(eco,total_area,z)
          val prop: VertexRDD[Double] = propensities(eco,es,1.0,"Natural",EcoUnit.degradationEquation)
          // sample the distribution by drawing a random number
          val rnd_x: Double = rnd.nextDouble( prop.reduce(_+_) )
          val vid: VertexId = S3Utils.selectVId(rnd_x,prop)
          // update composition accordingly
          val upd_eco: Graph[EcoUnit,Long] = updatedComposition(vid, "Degraded", eco)
          // update remaining number of land cover changes
          val upd_n_agr = n_agr
          if n_deg>0 { val upd_n_deg = n_deg - 1 }
          else {val upd_n_deg = n_deg}
        }
      }
      rec(upd_eco, plan, mng, total_area, z, upd_n_agr, upd_n_deg)  
    }

  }

}
