package model
import scala.math.pow
import org.apache.spark.*
import org.apache.spark.graphx.*
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph

import scala.annotation.tailrec
import scala.reflect._

/**
Extends a landscape composed by a graph of EcoUnits with ecosystem services functionalities. The trait serves to
retrieve a graph of ecosystem services flow and the natural connected components as a metric of fragmentation. All the
intermediate functions are located in the companion object.
 */
trait EcoServices :

  val size: Int
  val scalexp: Double
  val comp: Graph[EcoUnit, Long]

  /**
   * @return a tuple containing the natural connected components and graph of ecosystem services flow
   * */
  def ecosystemServiceFlow: (VertexRDD[VertexId], Graph[(EcoUnit,Double),Long]) =
    EcoServices.calculateEcoServices(comp,scalexp,size)

object EcoServices :

  /**
  @param eco is the biophysical landscape's composition graph
  @return the vertices in the connected components graph
  */
  def naturalConnectedComponents(eco: Graph[EcoUnit, Long]): VertexRDD[VertexId] =
    val natural = eco.subgraph(vpred = (_,eu) => eu.cover == LandCover.Natural)
    natural.connectedComponents().vertices

  /**
  @param ncc is the natural connected components, VertexRDD is over the EcoUnits and VertexId refers to the component Id
  @return a map with the number of units in each component
  */
  def nccAreaDistribution(ncc: VertexRDD[VertexId]): Map[(VertexId,VertexId), Long] =
    ncc.countByValue()

  /**
  @param size is the total number of EcoUnits in the landscape
  @param ncc_area is a map with the area of each natural connected component
  @return a biophysical landscape graph with information on the area of the ncc of each node
  */
  def nccAreaGraph(
    eco: Graph[EcoUnit, Long],
    ncc: VertexRDD[VertexId],
    ncc_area: Map[(VertexId,VertexId), Long],
    size: Double):
  Graph[(EcoUnit,Double), Long] =
    val area_vertices: VertexRDD[Double] = ncc.mapValues{
      (v,c) => ncc_area.getOrElse((v,c),-1L).toDouble
    }

    // Create a graph where each node attribute is the normalized area of the
    // natural component the ecological unit with such id belongs to. If the
    // vertex is not a natural cell, then put 0.0 as attribute.
    eco.outerJoinVertices[Double,(EcoUnit,Double)](area_vertices){
      (_, eu, va_opt) => va_opt match {
        case Some(vertex_area) => (eu,vertex_area/size)
        case None => (eu, 0.0)
      }
    }

  /**
  @param a is the area of the natural component
  @param z is the scaling exponent of the ecosystem services area relationship
  @return the value of ecosystem service provision for a component of area a
  */
  def esAreaRelation(
    a: Double,
    z: Double):
  Double =
    pow(a,z)

  /**
  @param area_graph is the biophysical composition of the landscape joined with the ncc area
  @param z is the scaling exponent of the ecosystem services area relationship
  @return a VertexRDD with the ecosystem service inflow as an attribute
  */
  def flow(
    area_graph: Graph[(EcoUnit,Double),Long],
    z: Double):
  VertexRDD[Double] =
    area_graph.aggregateMessages[(Int,Double)](
      triplet => {
        if (triplet.srcAttr._1.cover == LandCover.Natural) {
          // the second attribute is the component's area
          triplet.sendToDst((1,esAreaRelation(triplet.srcAttr._2, z)))
        }
        else triplet.sendToDst((1,0.0))
      },
      (a,b) => (a._1 + b._1, a._2 + b._2)
    ).mapValues( (_,value) => value._2/value._1 )

  /**
  @param eco is the composition of the ecological landscape
  @param scalexp is the exponent of the ecosystem services area relationsihp
  @param size is the size of the ecological landscape
  @param ncc is the natural connected components
  @return the ES flow in each ecological unit
  */
  def flowDirect(
    eco: Graph[EcoUnit,Long],
    scalexp: Double,
    size: Int,
    ncc: VertexRDD[VertexId]):
  VertexRDD[Double] =
    val ncc_area: Map[(VertexId,VertexId),Long] = nccAreaDistribution(ncc)
    val area_graph: Graph[(EcoUnit,Double),Long] = nccAreaGraph(eco,ncc,ncc_area,size)
    flow(area_graph,scalexp)

  /**
  @param ecocomp is the composition of the ecological landscape
  @param scalexp is the exponent of the ecosystem services area relationship
  @param size is the size of the ecological landscape
  @return a tuple with the natural connected components and the joined graph of ES flow
  */
  def calculateEcoServices(
    ecocomp: Graph[EcoUnit, Long],
    scalexp: Double,
    size: Int):
  (VertexRDD[VertexId], Graph[(EcoUnit,Double),Long] ) =
    val ncc: VertexRDD[VertexId] = naturalConnectedComponents(ecocomp)
    val es: VertexRDD[Double] = flowDirect(ecocomp,scalexp,size,ncc)
    (ncc, joinCompAndEcoServices(ecocomp,es))

  /**
  @return a graph joining the EcoUnits with the ES flow they receive
  */
  def joinCompAndEcoServices(
    comp: Graph[EcoUnit,Long],
    es: VertexRDD[Double]):
    Graph[(EcoUnit,Double),Long] =
     comp.outerJoinVertices(es){ (_, eu, es_opt) =>
       es_opt match {
         case Some(es) => (eu, es)
         case None => (eu, 0.0)
       }
     }

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
 /** def averageESFlow(
    eco: Graph[EcoUnit,Long],
    z: Double,
    size: Int):
    Double =
      esGraph(eco,z,size).vertices.reduce{ case ((v, a),(_,b)) => (v, a._2 + b._2) }._2 / size.toDouble 


  /**
  Fraction of natural habitat that needs to be removed with uniform probability to
  halve average ecosystem service provision
  */
  def robustnessESFlowOneReplica(average: Double,
                                 eco: Graph[EcoUnit,Long],
                                 z: Double,
                                 size: Int): Double = {

      val thr: Double = average * 0.5
      val vid: VertexId = rnd.shuffle( eco.subgraph(vpred = (_,eu) => eu.cover == Natural).vertices.collect() ).take(1)._1
      val new_eco: Graph[EcoUnit,Long] = eco.mapValues{ case (v,_) if v == vid => (v, Degraded) }
      val new_avg: Double = averageESFlow(new_eco,z,size)
      val n: Int = 1

      @tailrec
      def rec(thr: Double,
              current_avg: Double,
              eco: Graph[EcoUnit,Long],
              z: Double,
              size: Int,
              n: Int): Double = {
        if(current_avg <= thr) { n }
        else {
          val new_n: Int = n + 1
          val vid: VertexId = rnd.shuffle( eco.subgraph(vpred = (_,eu) => eu.cover == Natural).vertices.collect() ).take(1)._1
          val new_eco: Graph[EcoUnit,Long] = eco.mapValues{ case (v,_) if v == vid => (v, Degraded) }
          val new_avg: Double = averageESFlow(new_eco,z,size)
          rec(thr,new_avg,new_eco,z,size,new_n)
        }
      }
      rec(thr,new_avg,new_eco,z,size,n) / eco.subgraph(vpred = (_,eu) => eu.cover == Natural).vertices.count.toInt
  }

  def robustnessESFlow(average: Double,
                       eco: Graph[EcoUnit,Long],
                       z: Double,
                       size: Int,
                       n: Int): Double = {
    (0 until n).flatMap( _ => robustnessESFlowOneReplica(average,eco,z,size) ).reduce((a,b) => a + b)/n
  }
*/
end EcoServices
