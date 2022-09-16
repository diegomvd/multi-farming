package model
import scala.collection.immutable.ListMap
/*
import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph
*/
import scala.collection.parallel.immutable.ParVector
import scala.util.Random as rnd
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

/**
Implementation of the Management Unit. MngUnits are the elementary constituents of the management landscape and are
composed of PlnUnits referenced through their VertexId in the PlnLandscape. MngUnits are also defined by their management
strategy: land-sparing or land-sharing.
@note Composition and strategy are determined at initialization and, for the time being, cannot be changed over the
      course of a simulation.
*/
case class MngUnit(
                    id: Long,
                    composition: ParVector[Long],
                    strategy: MngStrategy
                  ):
  /**
  Determines if a MngUnit is available for conversion: at least one PlnUnit within the MngUnit must be available. Serves
  to build the probability of choosing any of the MngUnits of a MngLandscape: only available MngUnits can be chosen.
  @param pln the composition of the PlnLandscape that serves as base for the MngLandscape containing this MngUnit
  @param eco the composition of the EcoLandscape that serves as base for the PlnLandscape
  @return true if the unit is available, false if it isn't
  */
  def isAvailable(
                   pln: Graph[(Long,PlnUnit),UnDiEdge],
                   eco: Graph[(Long,EcoUnit),UnDiEdge]
                 ):
  Boolean =
    // searches for an planing unit within this management unit that is available considering the ecological landscape
    this.composition.exists{ pln_id => pln.nodes.toOuter.find( _._1 == pln_id )._2.isAvailable(eco) }

  /**
  Calculates the conversion propensity for each of the PlnUnits belonging to this MngUnit given this MngUnit's conversion
  propensity an initial propensity value and this MngUnit's strategy.
  @param i_val the initial value for the cumulative sum of the propensities
  @param u_tcp this MngUnit's total conversion propensity
  @param pln the PlnLandscape containing the PlnUnits from this MngUnit
  @param eco is the composition of the EcoLandscape serving as base for the PlnLandscape
  @return a ListMap with the cumulative propensities for each PlnUnit inside the MngUnit
  */
  def propensityOfPlnUnits(
                            i_val: Double,
                            u_tcp: Double,
                            pln: PlnLandscape,
                            eco: Graph[(Long,EcoUnit),UnDiEdge]
                          ):
  ListMap[PlnUnit,Double] =
    // this step is to calculate the individual propensities, sort them by vertexId and store in a ListMap
    val prop: ListMap[VertexId,Double] =
      ListMap(
        MngUnit.weights(this.composition,pln,eco,this.strategy).mapValues(_ * utcp).collect.toSeq.sortWith(_._1 < _._1):_*
      )
    // this step effectuates the cumulative sum starting with the initial value and yields the cumulative propensities scaled to the rest of the world's propensities
    prop.scanLeft[(VertexId,Double)]((-1L,ival))((pre, curr) => (curr._1, curr._2 + pre._2)).to(ListMap)

object MngUnit :
  /**
  @param comp the composition of this MngUnit
  @param pln the PlnLandscape containing the PlnUnits from this MngUnit
  @param eco the composition of the EcoLandscape that serves as base for the PlnLandscape
  @param stg is thi MngUnit's management strategy
  @return a VertexRDD with the relative conversion probability associated to each PlnUnit of this MngUnit
  */
  def weights(
               comp: ParVector[Long],
               pln: PlnLandscape,
               eco: Graph[(Long,EcoUnit),UnDiEdge],
               stg: MngStrategy
             ):
  Map[PlnUnit,Double] =

    def normalize(v: Map[PlnUnit,Double]): Map[PlnUnit,Double] =
      // normalization step
      // normally w should be strictly positive since this calculation would be never
      // done in an unavailable unit, this is being extra cautious and can be helpful for debugging
      val v_tot: Double = v.reduceLeft( (sum, map) => sum + map._2)
      if v_tot > 0.0 then v.map(_ / v_tot) else v

    // land-sparing units prefer to segregate new conversions from natural land
    // thus weighting more available units close to unavailable ones
    // land-sharing units prefer to integrate new conversions with natural land
    // thus weighting more available units close to available ones
    stg match {
      case MngStrategy.LandSparing =>
        normalize( pln.unavailableNeighbors(comp,eco).mapValues(PlnUnit.weightExpression(_, MngStrategy.LandSparing.clustering)) )
      case MngStrategy.LandSharing =>
        normalize( pln.availableNeighbors(comp,eco).mapValues(PlnUnit.weightExpression(_, MngStrategy.LandSharing.clustering)) )
    }

end MngUnit
