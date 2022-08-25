/**
The Planning Landscape is implemented in the PlnLandscape case class and companion
object. A Planning Landscape is described by its composition, the relative scale
to the EcoLandscape and its size which is the number of Planning Units, determined
by the size of the Ecological Landscape and the scale. The composition is a graph
of Planning Units where the existence of an edge between two units mean they are
adjacent.
The role of the Planning Landscape is to stock the adjacency between Planning Units
to be able to determine which units have un/available neighbors to determine the
conversion propensities.

@author diego

TODO: I am not usre that ajacency is determined correctly in the voronoi tesselation
because neighborhood in the EcoLandscape is functional connectivity rather than
adjacency
*/

import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph
import scala.math.pow
import scala.math.max

case class PlnLandscape(
  composition: Graph[PlnUnit,Long],
  scale: Double,
  size: Int)
  extends TopLandscape with BaseLandscape with SpatialStochasticEvents:

    /**
    @param mngunit is the sample of the PlanningLandscape (Management Unit) for
    which we want to determine neighbor availability
    @param eco is the composition of the base EcoLandscape
    @return the number of available neighbors for each PlanningUnit belonging to
    */
    def availableNeighbors(
      mngunit: ParVector[VertexId],
      eco: Graph[EcoUnit,Long]):
      VertexRDD[Int] =
        PlnLandscape.neighborAvailability(this.subLandscape(mngunit),eco,True)

    def unavailableNeighbors(
      mngunit: ParVector[VertexId],
      eco: Graph[EcoUnit,Long]):
      VertexRDD[Int] =
        PlnLandscape.neighborAvailability(this.subLandscape(mngunit),eco,False)

    /**
    @param mngunit is the sample of the PlanningLandscape (Management Unit) for
    which we want to determine neighbor availability
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
