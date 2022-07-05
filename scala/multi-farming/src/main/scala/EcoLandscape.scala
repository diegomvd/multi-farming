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
  * @param uid is the vertexId of the unit
  * @param cover is the new land cover
  * @param comp is the biophysical landscape composition
  * @return the updated composition
  */
  def updatedComposition(uid: Long,
                         cover: String
                         comp: Graph[EcoUnit, Long]) = Graph[EcoUnit, Long] {
    Graph( comp.vertices.mapValues{ case (uid,_) => cover }, comp.edges )
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
                   f: (Double,Double) => Double) = VertexRDD[Double] {

    val joined_comp: Graph[(EcoUnit,Double),Long] = joinES(comp,es)
    joined_comp.vertices.mapValues{ (id, (eui, esi)) => EcoUnit.propensity(esi, s, eui.matchCover(c), f) }
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
