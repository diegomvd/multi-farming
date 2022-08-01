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

case class PlnLandscape(
  composition: Graph[PlnUnit,Long]
  scale: Double,
  size: Int)
  extends TopLandscape with BaseLandscape with SpatialStochasticEvents:

    def availableNeighbors(eco: Graph[EcoUnit,Long]): VertexRDD[VertexId] =
      PlnLandscape.neighborAvailability(this.composition,eco,True)

    def unavailableNeighbors(eco: Graph[EcoUnit,Long]): VertexRDD[VertexId] =
      PlnLandscape.neighborAvailability(this.composition,eco,False)

    /**
    @return a sub graph of the planning landscape composition consisting on a
    given management unit
    */
    def subLandscape(mngunit: ParVector[VertexId]): Graph[PlnUnit,Long] =
      this.composition.subgraph( vpred = (vid,attr) => mngunit.contains(vid) )

object PlnLandscape :

  /**
  @param nu is the number of planning units to create
  @param eco is the biophysical landscape
  @return the composition graph of the planning landscape
  TODO: need to check these functions depending on tesselations
  */
  def buildComposition(
    nu: Int,
    eco: EcoLandscape):
    Graph[PlnUnit,Long] =
      eco.tesselate(nu).mapValues(PlnUnit(_))

  def apply(
    plnscale: Double,
    eco: EcoLandscape):
    PlnLandscape =
      val nu = TopLandscape.numberOfUnits(plnscale,eco.size)
      val comp = buildComposition(nu,eco)
      PlnLandscape(comp,plnscale,nu)

  /**
  @param comp is the composition graph of the selected management unit
  @param eco is the composition of the biophysical landscape
  @param bool determines if we are looking or available or unavailable ones
  @return the number of available/unavailable neighbors for each available unit
  TODO: must check if there is need to send 0 when dstAttr is not available
  */
  def neighborAvailability(
    comp: Graph[PlnUnit,Long],
    eco: Graph[EcoUnit,Long],
    available: Bool):
    VertexRDD[Int] =
      available match {
        case True => val v = (1, 0)
        case False => val v = (0, 1)
      }
      comp.aggregateMessages[Int](
        triplet => {
          if (triplet.dstAttr.isAvailable(eco)){
            if (triplet.srcAttr.isAvailable(eco)) {
              triplet.sendToDst(v._1)
            }
            else triplet.sendToDst(v._2)
          }
        },
        (a,b) => a + b
      )
end PlnLandscape
