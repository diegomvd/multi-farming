/**
The PlnLandscape object provides all the functions needed to interact with a
planning landscape represented by a graph of PlnUnits. Key functions are:
1- Build a planning landscape given the radius of the biophysical landscape
2- Determine neighbor availability to influence conversion weights
3- Get all the planning units within a management unit extended by their neighborhood
*/

import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph
import scala.math.pow
import scala.math.max

object PlnLandscape{

  /**
  @param nu is the number of planning units to create
  @param eco is the biophysical landscape
  @return the composition graph of the planning landscape
  TODO: the tesselation returns a Graph of iterables of vertex instead of vertexRDD, must check if this is a problem or if PLUnits could be VertexRDDs
  */
  def build(nu: Int,
            eco: Graph[EcoUnit,Long]): Graph[PlnUnit,Long] = {
    VoronoiUtils.tesselation(nu,eco).mapValues(PlnUnit(_))
  }

  /**
  @param comp is the composition graph of the selected management unit
  @param eco is the composition of the biophysical landscape
  @return the number of available neighbors for each available unit
  */
  def availableNeighbors(comp: Graph[PlnUnit,Long],
                         eco: Graph[EcoUnit,Long]): VertexRDD[Int] = {
    comp.aggregateMessages[Int](
      triplet => {
        if (triplet.dstAttr.isAvailable(eco)){
          if (triplet.srcAttr.isAvailable(eco)) {
            triplet.sendToDst(1)
          }
          else triplet.sendToDst(0)
        }
      },
      (a,b) => a + b
    )
  }

  /**
  @param comp is the composition graph of the selected management unit
  @param eco is the composition of the biophysical landscape
  @return the number of unavailable neighbors for each available unit
  */
  def unavailableNeighbors(comp: Graph[PlnUnit,Long],
                           eco: Graph[EcoUnit,Long]): VertexRDD[Int] = {
    comp.aggregateMessages[Int](
      triplet => {
        if (triplet.dstAttr.isAvailable(eco)) {
          if (triplet.srcAttr.isAvailable(eco)) {
            triplet.sendToDst(0)
          }
          else triplet.sendToDst(1)
        }
      },
      (a,b) => a + b
    )
  }

  /**
  * returns the subgraph of the planning landscape belonging to a management unit
  * with the adjacent pus, at the moment is not extended
  */
  def extendedSubGraph(comp: Graph[PlnUnit,Long],
                       sub: ParVector[VertexId]): Graph[PlnUnit,Long] = {
    comp.subgraph( vpred = (vid,attr) => sub.contains(vid) )
  }

}
