package model
import scala.math.pow

/*
import org.apache.spark.*
import org.apache.spark.graphx.*
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph
*/

import scala.annotation.tailrec
import scala.reflect._
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

/**
Extends a landscape composed by a graph of EcoUnits with ecosystem services functionalities. The trait serves to
retrieve a graph of ecosystem services flow and the natural connected components as a metric of fragmentation. All the
intermediate functions are located in the companion object.
 */
trait EcoServices :

  val size: Int
  val scal_exp: Double
  val comp: Graph[EcoUnit, UnDiEdge]

  /**
   * @return a map with the EcoUnit as key and their incoming ES flow as value
   * */
  def ecoServices: Map[EcoUnit, Double]  =
    val ncc = EcoServices.naturalConnectedComponents(this.comp)
    val ncm = EcoServices.nodeComponentMembership(ncc)
    val nam = EcoServices.nccNormalizedAreaMap(ncc,this.size.toDouble)
    val out = EcoServices.outgoingEcoServicePerUnit(ncm,nam,this.scal_exp)
    EcoServices.incomingEcoServicePerUnit(this.comp,out)

  /**
   *  @return the set of disconnected natural connected components
   */
  def naturalConnectedComponents: Map[Long, Graph[EcoUnit, UnDiEdge]] =
    this.comp.componentTraverser(subgraphNodes = n => n.matchCover(LandCover.Natural)).map(_.toGraph).zipWithIndex.map(_.swap).toMap

object EcoServices :

  /**
   * Creates a Map with EcoUnits as keys and NCC id as value.
   *
   * @param ncc the map of natural connected components
   * @return the ecounit-ncc map
   * */
  def nodeComponentMembership(ncc: Map[Long, Graph[EcoUnit, UnDiEdge]]): Map[EcoUnit, Long] =
    ncc.flatMap((id, graph) => graph.nodes.toList.map(unit => (unit, id)))

  /**
   * Creates a Map with the NCC id as key and its area in number of nodes as value
   * @param ncc the map of natural connected components
   * @return the map ncc-area
   * */
  def nccNormalizedAreaMap(
    ncc: Map[Long, Graph[EcoUnit, UnDiEdge]],
    size: Double):
  Map[Long,Long] =
    ncc.map( (id, graph) => (id, graph.nodes.size.toDouble/size) )

  /**
   *  @param a is the area of the natural component
   *  @param z is the scaling exponent of the ecosystem services area relationship
   *  @return the value of ecosystem service provision for a component of area a
   */
  def esAreaRelation(
    a: Double,
    z: Double):
  Double =
    pow(a, z)

  def outgoingEcoServicePerUnit(
    ncm: Map[EcoUnit,Long],
    nam: Map[Long,Long],
    scaling_exp: Double):
  Map[EcoUnit, Double] =
    ncm.map{ (unit, ncc) => (unit, esAreaRelation(nam.getOrElse(ncc,0.0), scaling_exp) ) }

  /**
   * Calculates the ecosystem service */
  def incomingEcoServicePerUnit(
    comp: Graph[EcoUnit, UnDiEdge],
    out: Map[EcoUnit, Double]):
  Map[EcoUnit, Double] =
    comp.get(1).innerNodeTraverser.foreach[EcoUnit, Double]{ n =>
      ( n,
        n.neighbors.foldLeft((0,0.0)){
          (tuple, neighbor) => (tuple._1 + 1, tuple._2 + out.getOrElse(neighbor,0.0) )
        }.map(_._2/_._1)
      )
    }.toMap

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
