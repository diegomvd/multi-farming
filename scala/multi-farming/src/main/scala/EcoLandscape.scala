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
  def build(r: Int,
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
  * @param vids are the vertexId of the units to update
  * @param cover is the new land cover
  * @param eco is the biophysical landscape composition
  * @return the updated composition
  */
  def updated(vids: VertexRDD[VertexId],
              cover: String
              eco: Graph[EcoUnit, Long]) = Graph[EcoUnit, Long] {
    val upd_vertices: VertexRDD = eco.vertices.mapValues{ case (vid, _) =>
      if vids.contains(vid) => EcoUnit(cover) } // note that the new cover is the same for every unit
    Graph( upd_vertices, eco.edges )
  }

  /**
  * @param uid is the vertexId of the unit
  * @param cover is the new land cover
  * @param eco is the biophysical landscape composition
  * @return the updated composition
  */
  def updated(uid: VertexId,
              cover: String
              eco: Graph[EcoUnit, Long]) = Graph[EcoUnit, Long] {
    val upd_vertices: VertexRDD = eco.vertices.mapValues{ case (vid, _) =>
      if (uid == vid) => EcoUnit(cover) } // note that the new cover is the same for every unit
    Graph( upd_vertices, eco.edges )
  }

  /**
  * @param eco is the biophysical landscape's composition
  * @return the vertices in the connected components graph
  */
  def naturalConnectedComponents(eco: Graph[EcoUnit, Long]) = VertexRDD[VertexId]{
    val natural = eco.subgraph(vpred = (vid,eu) => eu.cover == "Natural")
    val ncc = natural.connectedComponents().vertices
  }

  /**
  * @param ncc is the natural connected components, VertexRDD is over the EcoUnits
  * and VertexId refers to the component id
  * @return a map with the number of units in each component
  */
  def nccAreaDistribution(ncc: VertexRDD[VertexId]) = Map[(VertexId,VertexId), Long] {
    ncc.countByValue()
  }

  /**
  * @param size is the total number of EcoUnits in the landscape
  * @return a biophysical landscape graph with information on the area of the ncc of each node
  */
  def nccAreaGraph(eco: Graph[EcoUnit, Long],
                   ncc: VertexRDD[VertexId],
                   ncc_area: Map[(VertexId,VertexId), Long],
                   size: Double): Graph[(EcoUnit,Double), Long] = {
    val area_vertices: VertexRDD[Double] = ncc.map{ ncc_area.get((_,_)).toDouble }

    // join the area vertices and drop the ecounit
    eco.outerJoinVertices(area_vertices){ (id, _, av_opt) =>
      av_opt match {
        case Some(vertex_area) => (_,vertex_area/size)
        case None => (_, 0.0)
      }
    }
  }

  /**
  * @param eco_join is the biophysical composition of the landscape joined with the ncc area
  * @param z is the ES-area scaling exponent
  * @return an RDD with each unit and the ES flow they receive
  */
  def esFlow(eco_join: Graph[(EcoUnit,Double),Long],
             z: Double): VertexRDD[Double] = {
    eco_join.aggregateMessages[(Int,Double)](
      triplet => {
        if (triplet.srcAttr._1.cover == "Natural") {
          // the second attribute is the component's area
          triplet.sendToDst((1,pow(triplet.srcAttr._2, z)))
        }
        else triplet.sendToDst((1,0.0))
      },
      (a,b) => (a._1 + b._1, a._2 + b._2)
    ).mapValues( (id,val) => val._2/val._1 )
  }

  /**
  * @param eco is the biophysical composition of the landscape
  * @param es is the ecosystem service flow in each EcoUnit
  * @return a graph joining the ecounits with the ES flow they receive
  */
  def esGraph(eco: Graph[EcoUnit,Long],
              es: VertexRDD[Double]): Graph[EcoUnit,Long] = {
     eco.outerJoinVertices(es){ (vid, eu, es_opt) =>
       es_opt match {
         case Some(es) => (eu, es)
         case None => (eu, 0.0)
       }
     }
  }

  /**
  * @param eco is the biophysical composition of the landscape
  * @param es is the ecosystem service flow in each unit
  * @param s is this transition's sensitivity with es flow
  * @param c is the land cover type required for this transition
  * @param f is the function to calculate the propensity of this transition
  * @return a vertexRDD with the propensity for a certain transition in each EcoUnit of the graph
  */
  def propensities(eco: Graph[EcoUnit,Long],
                   es: VertexRDD[Double],
                   s: Double,
                   c: String,
                   f: (Double,Double,Bool) => Double) = VertexRDD[Double] {
    val es_graph: Graph[(EcoUnit,Double),Long] = esGraph(eco,es)
    es_graph.vertices.mapValues{ (id, (eu, esf)) => EcoUnit.propensity(esf, s, eu.matchCover(c), f) }
  }


  def initialize(eco: Graph[EcoUnit,Long],
                 plan: Graph[PlanningUnit,Long],
                 mng: Graph[ManagementUnit,Long],
                 size: Int,
                 z: Double,
                 fagr: Double,
                 fdeg: Double): Graph[EcoUnit,Long] = {

    val n_agr: Int = size*fagr.toInt
    val n_deg: Int = size*fdeg.toInt

    @tailrec
    def rec(eco: Graph[EcoUnit,Long],
            plan: Graph[PlanningUnit,Long],
            mng: Graph[ManagementUnit,Long],
            size: Int,
            z: Double,
            n_agr: Int,
            n_deg: Int): Graph[EcoUnit,Long] = {

      // this is the number of remaining transitions to execute
      val n: Int = n_agr + n_deg
      // for tail recursion the biophysical landscape is returned in case there are no more transitions to execute
      if (n==0){
        eco
      }
      else { // the recursive call, as long as there are transitions to execute, goes inside this block
        // the first step is selecting if next transition is agricultural expansion or land degradation
        val n_rnd: Int = rnd.nextInt(n)

        if (n_rnd < n_agr){ // this selects agricultural expansion
          // this might be wrong because 1.0 should be the total propensity
          val x_rnd: Double = rnd.nextDouble( 1.0 )
          val upd_eco: Graph[EcoUnit,Long] = World.applyConversionEvent(x_rand,eco,plan,mng,1.0)

          // update remaining number of land cover changes
          val upd_n_deg = n_deg
          if n_agr>0 { val upd_n_agr = n_agr - 1 }
          else {val upd_n_agr = n_agr}
        }
        else{ // this selects a land degradation transition
          // calculate ecosystem service flow and the degradation propensity
          val ncc: VertexRDD[VertexId] = naturalConnectedComponents(eco)
          val ncc_area: Map[(VertexId,VertexId),Long] = nccAreaDistribution(ncc)
          val area_graph: Graph[(EcoUnit,Double),Long] = nccAreaGraph(eco,ncc,ncc_area,size)
          val es_flow: VertexRDD[Double] = esFlow(area_graph,z)
          val prop: VertexRDD[Double] = propensities(eco,es_flow,1.0,"Natural",EcoUnit.degradationEquation)

          val x_rnd: Double = rnd.nextDouble( prop.reduce(_+_) )
          val upd_eco: Graph[EcoUnit,Long] = World.applySpontaneousEvent(x_rand,prop,eco,"Degraded")

          // update remaining number of land cover changes
          val upd_n_agr = n_agr
          if n_deg>0 { val upd_n_deg = n_deg - 1 }
          else {val upd_n_deg = n_deg}
        }
      }
      rec(upd_eco, plan, mng, size, z, upd_n_agr, upd_n_deg)
    }
  }

}
