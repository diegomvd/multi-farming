/**
The MngUnit class represents a management unit. Management units are composed by
a set of planning units stored as a VertexRDD that stores the VertexIds of the
contained planning units in the planning landscape's composition graph. Management
units are also defined by their management strategy encoded as a String.
Key functions are:
1- Determine availability for agricultural conversion in management unit
2- Determine conversion propensities for each planning unit inside the management unit
*/

import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph

case class MngUnit(comp: ParVector[VertexId], stg: String){

  def isAvailable(plan: Graph[PlnUnit,Long],
                  eco: Graph[EcoUnit,Long]): Bool = {
    MngUnit.isAvailable(this.comp,plan,eco)
  }

  def propensities(ival: Double,
                   utcp: Double,
                   pln: Graph[PlnUnit,Long],
                   eco: Graph[EcoUnit,Long]): VertexRDD[Double] = {
    MngUnit.propensities(ival,utcp,this.comp,pln,eco,this.stg)
  }
}

object MngUnit{

  /**
  The function checks if there is any planning unit inside this management unit
  that is available.
  @param comp is the set of planning unit ids belonging to this managament unit: a vertex of the management graph
  @param pln is the planning landscape
  @param eco is the biophysical landscape
  @return true if the management unit is available, false if not
  */
  def isAvailable(comp: ParVector[VertexId],
                  pln: Graph[PlnUnit,Long],
                  eco: Graph[EcoUnit,Long]) : Bool = {
    comp.exists{  pln.lookup(_).comp.isAvailable(eco) }
  }

  /**
  @param comp is the composition of the management unit
  @param pln is the planning landscape composition graph
  @param eco is the biophysical landscape composition graph
  @param stg is the management strategy of the unit
  @return a VertexRDD with the relative conversion probability associated to each PU
  */
  def weights(comp: ParVector[VertexId],
              pln: Graph[PlnUnit,Long],
              eco: Graph[EcoUnit,Long],
              stg: String): VertexRDD[Double] = {

    // the first thing is to get the extended subgraoh of the planning landscape:
    // planning units in the border of the management unit "see" the adjacent
    // planning units in other management units
    val pun = PlnLandscape.extendedSubGraph(plan,comp)

    // land-sparing units prefer to seggregate new conversions from natural land
    // thus weighting more available units close to unavailable ones
    // land-sharing units prefer to integrate new conversions with natural land
    // thus weighting more available units close to available ones
    stg match {
      case "Sparing" => val w = PlnLandscape.unavailableNeighbors(pun,eco).mapValues( PlnUnit.weightExpression(_,3.0) )
      case "Sharing" => val w = PlnLandscape.availableNeighbors(pun,eco).mapValues( PlnUnit.weightExpression(_,3.0) )
    }

    // normalization step
    // normally w should be strictly positive since this calculation would be never
    // done in an unavailable unit, this is being extra cautious and can be helpful for debugging
    val w_tot = w.reduce(_+_)
    w_tot match {
      case 0.0 => w
      case _ => w.mapValues(_ / w_tot)
    }
  }

  /**
  Given this unit's total conversion propensity and an initial propensity value
  we can calculate the cummulative propensity from the weights. Propensities
  are stored in ListMaps because order is fundamental to do the cummulative sum
  and then to randomly choose a unit according to the propensity distribution
  @param ival is the initial value for the cumulative propensities
  @param utcp is the unit's total conversion propensity
  @return a ListMap with the cumulative propensities for each PlnUnit inside the MngUnit
  */
  def propensities(ival: Double,
                   utcp: Double,
                   comp: ParVector[VertexId],
                   pln: Graph[PlnUnit,Long],
                   eco: Graph[EcoUnit,Long],
                   stg: String): ListMap[VertexId,Double] = {
    // this step is to calculate the individual propensities, sort them by vertexId and store in a ListMap
    val prop: ListMap[VertexId,Double] = ListMap(weights(comp,pln,eco,stg).mapValues(_ * utcp).collect.toSeq.sortWith(_._1 < _._1):_*)
    // this step effectuates the cummulative sum starting with the initial value and yields the cummulatie propensities scaled to the rest of the world's propensities
    prop.scanLeft((-1L,ival))((pre, k -> v) => k -> v + pre._2)
  }
}
