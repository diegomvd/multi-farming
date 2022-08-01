/**
This object provides all the functions needed to interact with a management
landscape represented by a graph of MngUnits. The key functions are:
1- Build the management landscape
*/

import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph

case class MngLandscape(
  composition: Graph[MngUnit,Long],
  scale: Double,
  nsparing: Int,
  size: Int)
  extends TopLandscape with SpatialStochasticEvents:

  /**
  @param ival is the initial value for the propensities
  @param tcp is the total conversion propensity
  @return a ListMap containing the choosing cummulative propensity for each management unit
  */
  def propensityOfMngUnits(
    ival: Double,
    tcp: Double,
    pln: Graph[PlnUnit, Long],
    eco: Graph[EcoUnit, Long]):
    ListMap[VertexId,Double] =
      val prop: ListMap[VertexId,Double] = ListMap(MngLandscape.probabilities(this.composition,pln,eco).mapValues(_ * tcp).collect.toSeq.sortWith(_._1 < _._1):_*)
      prop.scanLeft((-1L,ival))( (pre, k -> v) => k -> v + pre._2 )

object MngLandscape :

  /**
  @param nm is the number of management units
  @param pln is the planning landscape composition
  @param fs is the fractions of land-sparing MngUnits
  @return a map storing each management unit's composition and their ids
  TODO: need to check this functions depending on tesselation
  **/
  def apply(
    mngscale: Double,
    pln: PlnLandscape,
    fs: Double):
    MngLandscape =
      val nu = TopLandscape.numberOfUnits(mngscale,pln.size)
      val tess_graph: Graph[ParVector[PlnUnit],Long] = pln.tesselate(nm)
      val nsparing = fs * nm
      val sparing_ids: VertexRDD[ParVector[PlnUnit]] = rnd.shuffle(tess_graph.vertices).take((fs * nsparing).toInt)
      val comp = tess_graph.mapValues{ (vid,vec) =>
        if sparing_ids.contains((vid,vec)) { MngUnit(vec,LandSparing) }
        else { MngUnit(vec,LandSharing) }
      }
      MngLandscape(comp,mngscale,nsparing,nu)

  /**
  @param mng the management landscape composition
  @param pln the planning landscape
  @param eco the biophyisical landscape
  @return an RDD with the relative conversion probabilities at each management unit. We assume uniform probability among the available units
  */
  def probabilities(
    mng: Graph[MngUnit, Long],
    pln: Graph[PlnUnit, Long],
    eco: Graph[EcoUnit, Long]):
    VertexRDD[Double] =
      val sg: Graph[MngUnit, Long] = mng.subgraph(vpred = (vid,mu) => mu.isAvailable(pln,eco))
      sg.vertices.mapValues(_ / sg.count)

end MngLandscape
