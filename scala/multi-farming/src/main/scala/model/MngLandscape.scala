package model
import scala.collection.immutable.ListMap
import org.apache.spark.*
import org.apache.spark.graphx.*
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph

import scala.collection.parallel.immutable.ParVector
import scala.util.Random as rnd
import scala.reflect._

/**
Implementation of the Management Landscape, composed by Management Units. A MngLandscape is extends a TopLandscape and
is thus defined by its composition, size and scale. Additionally the class has a "nsparing" parameter that represents
the number of Management Units f the Management Landscape that apply a land-sparing strategy.
@note The Management Landscape is instantiated at initialization and, for the time being, changes in the Management
      Landscape during a simulation are not implemented.
@author diego
*/
case class MngLandscape(
  composition: Graph[MngUnit,Long],
  scale: Double,
  nsparing: Int,
  size: Int)
  extends TopLandscape[MngUnit] with SpatialStochasticEvents:
  /**
   * Calculates the propensity of choosing each MngUnit for agricultural expansion.
   * @param ival initial value for the cumulative sum of conversion propensities in each MngUnit
   * @param tcp total conversion propensity determined by resource demand
   * @return a ListMap containing cumulative propensity for choosing each management unit
  */
  def propensityOfMngUnits(
    ival: Double,
    tcp: Double,
    pln: Graph[PlnUnit, Long],
    eco: Graph[EcoUnit, Long]):
  ListMap[VertexId,Double] =
    val prop: ListMap[VertexId,Double] = ListMap(MngLandscape.probabilities(this.composition,pln,eco).mapValues(_ * tcp).collect.toSeq.sortWith(_._1 < _._1):_*)
    prop.scanLeft[(VertexId,Double)]((-1L,ival))( (pre, curr) => (curr._1, curr._2 + pre._2) ).to(ListMap)

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
    val tess_graph: Graph[ParVector[VertexId],Long] = pln.tesselate(nm)
    val n_sparing = fs * nm
    val sparing_ids: Array[(VertexId,ParVector[VertexId])] = rnd.shuffle(tess_graph.vertices.collect).toArray.take( (fs * n_sparing).toInt )
    val comp = tess_graph.mapVertices{ (vid,vec) =>
      if sparing_ids.contains((vid,vec)) then MngUnit(vec,MngStrategy.LandSparing)  else MngUnit(vec,MngStrategy.LandSharing)
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
    mng: Graph[MngUnit, Long],
    pln: Graph[PlnUnit, Long],
    eco: Graph[EcoUnit, Long]):
  VertexRDD[Double] =
    val sg: Graph[MngUnit, Long] = mng.subgraph(vpred = (_,mu) => mu.isAvailable(pln,eco))
    sg.vertices.mapValues( (_,_) => 1.0 / sg.vertices.count )

end MngLandscape
