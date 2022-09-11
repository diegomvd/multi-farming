package model
import scala.collection.immutable.ListMap
/*
import org.apache.spark.*
import org.apache.spark.graphx.*
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph
*/
import scala.collection.parallel.immutable.ParVector
import scala.util.Random as rnd
import scala.reflect._
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

/**
Implementation of the Management Landscape, composed by Management Units. A MngLandscape is extends a TopLandscape and
is thus defined by its composition, size and scale. Additionally the class has a "nsparing" parameter that represents
the number of Management Units f the Management Landscape that apply a land-sparing strategy.
@note The Management Landscape is instantiated at initialization and, for the time being, changes in the Management
      Landscape during a simulation are not implemented.
@author diego
*/
case class MngLandscape(
  composition: Graph[(Long,MngUnit),UnDiEdge],
  scale: Double,
  n_sparing: Int,
  size: Int)
  extends TopLandscape[MngUnit] with SpatialStochasticEvents:
  /**
   * Calculates the propensity of choosing each MngUnit for agricultural expansion.
   * @param ival initial value for the cumulative sum of conversion propensities in each MngUnit
   * @param tcp total conversion propensity determined by resource demand
   * @return a ListMap containing cumulative propensity for choosing each management unit
  */
  def propensityOfMngUnits(
    i_val: Double,
    tcp: Double,
    pln: Graph[(Long,PlnUnit),UnDiEdge],
    eco: Graph[(Long,EcoUnit),UnDiEdge]):
  ListMap[MngUnit,Double] =
    val propensities: Map[MngUnit,Double] = MngLandscape.probabilities(this.composition,pln,eco)
    propensities.scanLeft((propensities.head._1, i_val))( (pre, now) => (now._1, now._2 + pre._2)).to(ListMap)

object MngLandscape :
  /**
   * MngLandscape constructor
   * @constructor
   * @param scale the relative scale of this management landscape to the planning landscape
   * @param pln the planning landscape serving as base for this management landscape
   * @param fs the fraction of land-sparing MngUnits
   * @return an instance of MngLandscape
   *
   * @todo need to check this function depending on tesselation
   */
  def apply(
    scale: Double,
    pln: PlnLandscape,
    fs: Double):
  MngLandscape =
    val nm = TopLandscape.numberOfUnits(scale,pln.size)
    val tess_graph: Graph[ParVector[Long],UnDiEdge] = pln.tesselate(nm)
    val n_sparing = fs * nm
    val sparing_ids: List[ParVector[Long]] = rnd.shuffle(tess_graph.nodes.toOuter.toList).take( (fs * n_sparing).toInt )
    val comp = tess_graph.zipWithIndex.map{ (x,id) =>
      //TODO: idk how map functions i.e. how to preserve edges and change node values
      if sparing_ids.contains(x.value)
      then (id,MngUnit(id,x.value,MngStrategy.LandSparing))
      else (id,MngUnit(id,x.value,MngStrategy.LandSharing))
    }
    MngLandscape(comp,scale,n_sparing.toInt,nm)

  /**
   *  Calculate the relative probabilities for each MngUnit to be selected for a conversion event.
   * @param mng the management landscape's composition
   * @param pln the planning landscape's composition
   * @param eco the ecological landscape's composition
   * @return an RDD with the relative conversion probabilities of each management unit
   * @note At the current modeling stage MngUnits are selected with uniform probability
  */
  def probabilities(
    mng: Graph[(Long,MngUnit), UnDiEdge],
    pln: Graph[(Long,PlnUnit), UnDiEdge],
    eco: Graph[(Long,EcoUnit), UnDiEdge]):
  Map[MngUnit,Double] =
    val available_units = mng.nodes.toOuter.filter( _._2.isAvailable(pln,eco) )
    available_units.map( tuple => (tuple._2, 1.0/available_units.size) ).toMap

end MngLandscape
