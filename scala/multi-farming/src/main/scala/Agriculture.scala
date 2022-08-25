import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph

trait Agriculture:

  /**
  @param yes is the contribution of ES to resource production in LI units
  @param his is the number of households that can be supported by the production of one HI unit
  */
  val yes: Double
  val his: Double

  // function for every other landscape transforming transition
  def resourceProduction(esgraph: Graph[(EcoUnit,Double), Long]): Double =
    Agriculture.calculateProduction(ecocomp,yes,his)



object Agriculture:

  /**
  @param es_graph is the biophysical composition of the landscape joined with the es flow
  @param y1
  @param y2
  @return the total amount of resources produced in the low-intensity units
  */
  def lowIntResources(
    es_graph: Graph[(EcoUnit,Double),Long],
    y1: Double,
    y2: Double):
    Double =
      // this function creates a subgraph and then traverses it, another option would be
      // to traverse the whole graph and match at each node agains the cover to calculate production
      // i am not sure what is the optimal. Creating a subgraph seems expensive, but at the
      // same time the subgraph function might be optimized within spark
      val low_intensity = es_graph.subgraph(vpred = (vid,(eu,x)) => eu.cover == LowIntensity )
      low_intensity.vertices.mapValues{ case (eu, es) => EcoUnit.lowIntResEquation(y1,y2,es) }.reduce( _+_ )


  /**
  @return the total amount of resources produced in the high-intensity units
  */
  def highIntResources(es_graph: Graph[(EcoUnit,Double),Long]): Double = {
    // this functions follows the same approach as the low intensity one. However,
    // due to non-dimensionalization in this model the total high intensity production
    // is just the number of high-intensity units, maybe it is better to just do that
    // trade off between clarity of code and execution time
    val high_intensity = es_graph.subgraph(vpred = (vid,(eu,_)) => eu.cover == HighIntensity)
    high_intensity.vertices.mapValues{ case (eu, es) => EcoUnit.highIntResEquation() }.reduce( _+_ )
  }

  /**
  @return the total amount of resources produced in the landscape
  */
  def calculateProduction(
    es_graph: Graph[(EcoUnit,Double),Long],
    y1: Double,
    y2: Double): Double = {
    lowIntResources(es_graph,y1,y2) + highIntResources(es_graph)
  }
end Agriculture
