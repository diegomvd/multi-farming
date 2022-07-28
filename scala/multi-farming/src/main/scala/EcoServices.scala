import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph
import scala.math.pow

trait EcoServices :

  /**
  @param scalexp is the ES-area scaling exponent
  @param ncc is the natural connected components in the landscape that serve as
             a base for ES calculation
  @param es is the resulting ES provision from the NCC and scalexp.
  Note that resulting ES are stored as a field because it does not necessarily
  require a recalculation after a landscape update. Although the most heavy
  calculation is for the NCC, we also store ES in the hope of optimizing speed.
  This is a compromise between elegance and efficiency.
  */
  val scalexp: Double
  val ncc: VertexRDD[VertexId]
  val es: VertexRDD[Double]

  def updateEcoServices(
    ecocomp: Graph[EcoUnit, Long],
    scalexp: Double,
    size: Int): (VertexRDD[Double], VertexRDD[VertexId]) = {

    val ncc: VertexRDD[VertexId] = EcoServices.naturalConnectedComponents(ecocomp)
    (ncc, EcoServices.flowDirect(ecocomp,scalexp,size,ncc))
  }

object EcoServices :

  /**
  @param eco is the biophysical landscape's composition graph
  @return the vertices in the connected components graph
  */
  def naturalConnectedComponents(eco: Graph[EcoUnit, Long]): VertexRDD[VertexId] = {
    val natural = eco.subgraph(vpred = (vid,eu) => eu.cover == Natural)
    val ncc = natural.connectedComponents().vertices
  }

  /**
  @param ncc is the natural connected components, VertexRDD is over the EcoUnits and VertexId refers to the component Id
  @return a map with the number of units in each component
  */
  def nccAreaDistribution(ncc: VertexRDD[VertexId]): Map[(VertexId,VertexId), Long] = {
    ncc.countByValue()
  }

  /**
  @param size is the total number of EcoUnits in the landscape
  @param ncc_area is a map with the area of each natural connected component
  @return a biophysical landscape graph with information on the area of the ncc of each node
  */
  def nccAreaGraph(eco: Graph[EcoUnit, Long],
                   ncc: VertexRDD[VertexId],
                   ncc_area: Map[(VertexId,VertexId), Long],
                   size: Double): Graph[(EcoUnit,Double), Long] = {
    val area_vertices: VertexRDD[Double] = ncc.mapValues{case (uid,cid) => ncc_area.get((uid,cid)).toDouble }
    // Create a graph where each node attribute is the normalized area of the
    // natural component the ecological unit with such id belongs to. If the
    // vertex is not a natural cell, then put 0.0 as attribute.
    eco.outerJoinVertices(area_vertices){ (id, _, av_opt) =>
      av_opt match {
        case Some(vertex_area) => (_,vertex_area/size)
        case None => (_, 0.0)
      }
    }
  }

  /**
  @param a is the area of the natural component
  @param z is the scaling exponent of the ecosystem services area relationship
  @return the value of ecosystem service provision for a component of area a
  */
  def esAreaRelation(a: Double,
                     z: Double): Double = {
    pow(a,z)
  }

  /**
  @param area_graph is the biophysical composition of the landscape joined with the ncc area
  @param z is the scaling exponent of the ecosystem services area relationship
  @return a VertexRDD with the ecosystem service inflow as an attribute
  */
  def flow(area_graph: Graph[(EcoUnit,Double),Long],
           z: Double): VertexRDD[Double] = {
    area_graph.aggregateMessages[(Int,Double)](
      triplet => {
        if (triplet.srcAttr._1.cover == "Natural") {
          // the second attribute is the component's area
          triplet.sendToDst((1,esAreaRelation(triplet.srcAttr._2, z)))
        }
        else triplet.sendToDst((1,0.0))
      },
      (a,b) => (a._1 + b._1, a._2 + b._2)
    ).mapValues( (id,val) => val._2/val._1 )
  }

  def flowDirect(eco: Graph[EcoUnit,Long],
                 scalexp: Double,
                 size: Int,
                 ncc: VertexRDD[VertexId]): VertexRDD[Double] = {
    val ncc_area: Map[(VertexId,VertexId),Long] = nccAreaDistribution(ncc)
    val area_graph: Graph[(EcoUnit,Double),Long] = nccAreaGraph(eco,ncc,ncc_area,size)
    flow(area_graph,scalexp)
  }
end EcoServices
