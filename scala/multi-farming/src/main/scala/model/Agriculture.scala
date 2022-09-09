package model
/*
import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph
*/

import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

/**
 * Implementation of Agriculture: a trait used to extend a Landscape.
 * @todo check mapValues and reduce
 */
trait Agriculture:
  /**
  yes is the contribution of ES to resource production in LI units
  his is the number of households that can be supported by the production of one HI unit
  */
  val yes: Double
  val his: Double
  def resourceProduction(es_graph: Graph[(EcoUnit,Double), UnDiEdge]): Double =
    Agriculture.calculateProduction(es_graph,yes,his)

object Agriculture:
  /**
  @param es_graph is the biophysical composition of the landscape joined with the es flow
  @param yes is the contribution of ecosystem services to low-intensity agricultural production
  @param his is the number of households that are maintained with the production of one high-intensity unit
  @return the total amount of resources produced in the low-intensity units
  */
  def lowIntResources(
    es_graph: Graph[(EcoUnit,Double), UnDiEdge],
    yes: Double,
    his: Double):
  Double =
    // this function creates a subgraph and then traverses it, another option would be
    // to traverse the whole graph and match at each node against the cover to calculate production
    // i am not sure what is the optimal. Creating a subgraph seems expensive, but at the
    // same time the subgraph function might be optimized within spark
    val low_intensity : Graph[(EcoUnit,Double),UnDiEdge] = es_graph.subgraph( vpred = (_,attr) => attr._1.cover == LandCover.LowIntensity )
    low_intensity.vertices.mapValues{ case (_, es) => EcoUnit.lowIntResEquation(yes,his,es) }.reduce( (v1,v2) => (0L, v1._2 + v2._2) )._2

  /**
  @return the total amount of resources produced in the high-intensity units
  */
  def highIntResources(es_graph: Graph[(EcoUnit,Double), UnDiEdge]): Double =
    // this functions follows the same approach as the low intensity one. However,
    // due to non-dimensionalization in this model the total high intensity production
    // is just the number of high-intensity units, maybe it is better to just do that
    // trade off between clarity of code and execution time
    val high_intensity = es_graph.subgraph(vpred = (_,attr) => attr._1.cover == LandCover.HighIntensity )
    high_intensity.vertices.mapValues{ case (_,_) => EcoUnit.highIntResEquation() }.reduce( (v1,v2) => (0L, v1._2 + v2._2) )._2

  /**
  @return the total amount of resources produced in the landscape
  */
  def calculateProduction(
    es_graph: Graph[(EcoUnit,Double), UnDiEdge],
    yes: Double,
    his: Double): Double =
      lowIntResources(es_graph,yes,his) + highIntResources(es_graph)

end Agriculture
