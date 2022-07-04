import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph
import scala.math.pow

case class BioPhyLandscape(comp: Graph[String, Long],
                           ncc: ,
                           es: ){
  /**
  * @param s is the sensitivity to ecosystem service provision
  * @param c is the land cover type to be matched
  * @param f is the function yielding a propensity given ecosystem service
  *          provision and sensitivity
  */
  def propensities(s: Double, c: String, f: (Double,Double) => Double) = ParMap[ModCo,Double]{
    BioPhyLandscape.propensities(this.comp,this.es,s,c,f)
  }

  def updated(unit: EcoUnit): BioPhyLandscape =
    copy(this.radius, BioPhyLandscape.updatedComposition(unit,this.comp))

  /**
  Queries about the landscape's state
  **/
  def fractionNatural():       Double = this.comp.count{ _._2.isNatural() }
  def fractionDegraded():      Double = this.comp.count{ _._2.isDegraded() }
  def fractionLowIntensity():  Double = this.comp.count{ _._2.isLowIntensity() }
  def fractionHighIntensity(): Double = this.comp.count{ _._2.isHighIntensity() }

}

object BioPhyLandscape{

  /**
  * @param r is the radius of the biophysical landscape
  * @param ecr is the ecological connectivity range
  * @return a fresh, pristine biophysical landscape
  */
  def apply(r: Int,
            ecr: Int) = BioPhyLandscape {
    val comp = buildComposition(r,ecr)
    BioPhyLandscape(comp)
  }

  /**
  * @param r is the radius of the biophysical landscape
  * @param ecr is the ecological connectivity range
  * @return a biophysical composition with every unit in a natural state
  */
  def buildComposition(r: Int, ecr: Int) = Graph[String, Long] {

    val sc: SparkContext
    val units: RDD[(VertexId, String)] =
      sc.parallelize( ModCo.apply(r).map{ (_.toLong,"Natural") }.toSeq )
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
  def buildESFlow(r: Int) = ParMap[ModCo,Double] {
    ModCo.apply(r).map{ _ -> 1.0}
  }

  /**
  * @param uid is the vertexId of the unit
  * @param cover is the new land cover
  * @param comp is the biophysical landscape composition
  * @return the updated composition
  */
  def updatedComposition(uid: Long,
                         cover: String
                         comp: Graph[String, Long]) = Graph[String, Long] {
    Graph( comp.vertices.mapValues{ case (uid,_) => cover }, comp.edges )
  }

  /**
  * @param comp biophysical landscape's composition
  * @return the vertices in the connected components graph
  */
  def naturalConnectedComponents(comp: Graph[String, Long]) = VertexRDD[VertexId]{
    val natural = comp.subgraph(vpred = cover => cover == "Natural")
    val ncc = natural.connectedComponents().vertices
  }

  /**
  * @param ncc is the natural connected components
  * @return a map with the number of units in each component
  */
  def nccArea(ncc: VertexRDD[VertexId]) = Map[(VertexId,VertexId), Long] {
    ncc.countByValue()
  }

  /**
  */
  def joinNCCArea(comp: Graph[String, Long],
                  total_area: Double): Graph[String, Long] = {
    val ncc = naturalConnectedComponents(comp)
    val area = nccArea(ncc)
    val v_area: VertexRDD[Double] = ncc.map{ area.get((_,_)).toDouble }

    comp.outerJoinVertices(v_area){ (id, cover, v_area_opt) =>
      v_area_opt match {
        case Some(v_area) => (cover, v_area/total_area)
        case None => (cover,0.0)
      }
    }
  }

  /**
  * @param comp is the biophysical composition of the landscape
  * @param z is the ES-area scaling exponent
  * @return an RDD with each unit and the ES flow they receive
  */
  def updatedESFlow(comp: Graph[String,Long],
                    total_area: Double,
                    z: Double): VertexRDD[Double] = {

    val joined_comp: Graph[String, Long] = joinNCCArea(comp,total_area)

    joined_comp.aggregateMessages[(Int,Double)](
      triplet => {
        if (triplet.srcAttr._1 >= "Natural") {
          // this assumes that the second attribute is the component's area
          triplet.sendToDst((1,pow(triplet.srcAttr._2.toDouble, z)))
        }
        else triplet.sendToDst((1,0.0))
      },
      (a,b) => (a._1 + b._1, a._2 + b._2)
    ).mapValues( (id,val) => val._2/val._1 )
  }

  def joinES(comp: Graph[String,Long],
             es: VertexRDD[Double]): Graph[String,Long] = {
     comp.outerJoinVertices(es){ (id, cover, es_opt) =>
       es_opt match {
         case Some(es) => (cover, es)
         case None => (cover, 0.0)
       }
     }
  }

  def propensities(comp: Graph[String,Long],
                   es: VertexRDD[Double],
                   s: Double,
                   c: String,
                   f: (Double,Double) => Double) = VertexRDD[Double] {

    val joined_comp: Graph[String,Long] = joinES(comp,es)
    joined_comp.mapValues{ (id, val) => EcoUnit.propensity(val._2, s, EcoUnit.matchCover(c,val._1), f) }
  }

  // this is a discrete time stochastic process to generate an initial landscape
  // faithful to the mechanisms of the model in the hope that the transient duration
  def initialize(biophy: BioPhyLandscape,
                 plan: PlanningLandscape,
                 mng: ManagementLandscape,
                 n_agr: Int,
                 n_deg: Int): BioPhyLandscape = {
    // randomly choose either an agr or deg transition according to the remaining ones
    // if it is a deg transition: get weights of each vertex with deg propensity and es
    // update the landscape and move on
    // else choose randomly a management unit and then a planning unit with the given weights
    // update the landscape and move on
    // implement tail recursion
  }

}
