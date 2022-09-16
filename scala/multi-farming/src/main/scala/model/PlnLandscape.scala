package model
/*
import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph
*/

import scala.math.pow
import scala.math.max
import scala.collection.parallel.immutable.ParVector
import scala.util.Random as rnd
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

/**
Implementation of the Planning Landscape. A PlnLandscape is on the top of an EcoLandscape and at the base of a
MngLandscape. It is composed by a collection of PlnUnits.

@author diego

@todo I am not sure that adjacency is determined correctly in the voronoi tesselation because neighborhood in the
      EcoLandscape is functional connectivity rather than adjacency
*/
case class PlnLandscape(
                         composition: Graph[(Long,MngUnit),UnDiEdge],
                         scale: Double,
                         size: Int
                       )
  extends TopLandscape[PlnUnit] with BaseLandscape[PlnUnit] with SpatialStochasticEvents:

    /**
     * Calculates the number of available neighbors of each PlnUnit in the selected MngUnit. Used to determine the
     * PlnUnits conversion weights in a land-sharing MngUnit.
     * @param mng_unit the MngUnit's composition: a region of this PlnLandscape for which we want to determine neighbor
     *                 availability.
     * @param eco the composition of the EcoLandscape at the base of this PlnLandscape.
     * @return a VertexRDD with the number of available neighbors.
    */
    def availableNeighbors(
                            mng_unit: ParVector[Long],
                            eco: Graph[(Long,EcoUnit),UnDiEdge]
                          ):
    List[Int] =
      PlnLandscape.neighborAvailability(this.subLandscape(mng_unit),eco,true)

    /**
     * Calculates the number of unavailable neighbors of each PlnUnit in the selected MngUnit. Used to determine the
     * PlnUnits conversion weights in a land-sparing MngUnit.
     * @param mng_unit the MngUnit's composition: a region of this PlnLandscape for which we want to determine neighbor
     *                 availability.
     * @param eco the composition of the EcoLandscape at the base of this PlnLandscape.
     * @return a VertexRDD with the number of available neighbors.
     */
    def unavailableNeighbors(
                              mng_unit: ParVector[Long],
                              eco: Graph[(Long, EcoUnit), UnDiEdge]
                            ):
    List[Int] =
      PlnLandscape.neighborAvailability(this.subLandscape(mng_unit),eco,false)

    /**
     * Selects a part of this PlnLandscape corresponding to the composition of a MngUnit.
     * @param mng_unit the MngUnit's composition: a region of this PlnLandscape for which we want to determine neighbor
     *                 availability.
     * @return a sub graph of this PlnLandscape's composition.
    */
    def subLandscape(
                      mng_unit: ParVector[Long]
                    ):
    Graph[(Long,PlnUnit),UnDiEdge] =
      this.composition.filter( x => mng_unit.exists(_ == x.value) )

object PlnLandscape :
  /**
   * Builds the composition of a PlnLandscape from the base landscape and a number of PlnUnits to be created.
   * @param nu the number of planning units to create
   * @param eco the EcoLandscape at the base of the to be created PlnLandscape
   * @return a PlnLandscape composition graph.
   * @todo need to check these functions depending on tesselation. Update to scala-graph still uncertain, need to work out
   *       the voronoi tesselation first
  */
  def buildComposition(
                        nu: Int,
                        eco: EcoLandscape
                      ):
  Graph[(Long,PlnUnit),UnDiEdge] =
    eco.tesselate(nu).mapVertices( (_,vec) => PlnUnit(vec) )

  /**
   * Builds a PlnLandscape given its base landscape and its relative scale
   * @constructor
   * @param scale the relative scale of the PlnLandscape to the EcoLandscape.
   * @param eco the EcoLandscape at the base of the PlnLandscape.
   * @return a PlnLandscape.
   * */
  def apply(
             scale: Double,
             eco: EcoLandscape
           ):
  PlnLandscape =
    val nu = TopLandscape.numberOfUnits(scale,eco.size)
    val comp = buildComposition(nu,eco)
    PlnLandscape(comp,scale,nu)

  /**
   * Determines the un/availability of the neighbors of each PlnUnit in the selected MngUnit.
   * @param comp the composition graph of the part of the PlnLandscape corresponding to the selected MngUnit.
   * @param eco the composition of the EcoLandscape at the base of this PlnLandscape.
   * @param available specifies if the function should look for available or unavailable neighbors.
   * @return the number of available/unavailable neighbors for each available unit.
   * @todo must check if there is need to send 0 when dstAttr is not available
  */
  def neighborAvailability(
                            comp: Graph[(Long,PlnUnit),UnDiEdge],
                            eco: Graph[(Long,EcoUnit),UnDiEdge],
                            available: Boolean
                          ):
  List[Int] =

    def valueTuple(b: Boolean) = if b then (1, 0) else (0, 1)
    val v = valueTuple(available)

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
