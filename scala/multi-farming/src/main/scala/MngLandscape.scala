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

object MngLandscape{

  /**
  @param nm is the number of management units
  @param pln is the planning landscape composition
  @param fs is the fractions of land-sparing MngUnits
  @return a map storing each management unit's composition and their ids
  **/
  def build(nm: Int,
            pln: Graph[PlnUnit,Long],
            fs: Double): Graph[MngUnit,Long] = {
    val tess_graph: Graph[Iterable[PlnUnit],Long] = VoronoiUtils.tesselation(nm,pln)
    val sparing_ids: VertexRDD[Iterable[PlnUnit]] = rnd.shuffle(tess_graph.vertices).take((fs * nm).toInt)
    tess_graph.mapValues{ (vid,it) =>
      if sparing_ids.contains((vid,it)) { MngUnit(it,"Sparing") }
      else { MngUnit(it,"Sharing") }
    }
  }

  /**
  @param mng the management landscape composition
  @param pln the planning landscape
  @param eco the biophyisical landscape
  @return an RDD with the relative conversion probabilities at each management unit. We assume uniform probability among the available units
  */
  def probabilities(mng: Graph[MngUnit, Long],
                    pln: Graph[PlanningUnit, Long],
                    eco: Graph[EcoUnit, Long]): VertexRDD[Double] = {
    val sg: Graph[MngUnit, Long] = mng.subgraph(vpred = (vid,mu) => mu.isAvailable(pln,eco))
    sg.vertices.mapValues(_ / sg.count)
  }

  /**
  @param ival is the initial value for the propensities
  @param tcp is the total conversion propensity
  @return a ListMap containing the choosing cummulative propensity for each management unit
  */
  def propensities(ival: Double,
                   tcp: Double,
                   mng: Graph[MngUnit, Long],
                   pln: Graph[PlanningUnit, Long],
                   eco: Graph[EcoUnit, Long]): ListMap[VertexId,Double] = {
    val prop: ListMap[VertexId,Double] = ListMap(probabilities(mng,pln,eco).mapValues(_ * tcp).collect.toSeq.sortWith(_._1 < _._1):_*)
    prop.scanLeft((-1L,ival))( (pre, k -> v) => k -> v + pre._2 )
  }

}
