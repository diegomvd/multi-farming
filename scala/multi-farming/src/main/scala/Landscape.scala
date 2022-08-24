/**
This file contains the implementation of landscape traits.
@author diego
*/

import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph

/**
A landscape necessariliy has a composition and structure and a size. It also has
update composition functions for a single unit or several. This update composition
feature is only used in the EcoLandscape.
*/
trait Landscape:

  val composition: Graph[A, Long]
  val size: Int

  def updateComposition(a: A, b: A): Graph[A, Long] =
    composition.mapValues(case a => b)

  def updateComposition(a: VertexRDD[A], b: A): Graph[A, Long] =
    composition.mapValues{ case (vid,attr) if a.contains((vid,attr)) => b }

/**
A base landscape is a landscape over which a voronoi tesselation can be performed
in a way that it becomes the base of another landscape. Groups of units of the
primitive landscape are the elementary units of the secondary landscape and so on.
*/
trait BaseLandscape extends Landscape with VoronoiTesselation

/**
A top landscape is a landscape built on top of a base landscape given its relative
scale to the base landscape. A top landscape can also be a base landscape, as it
is the case currently with the planning landscape. This feature allows for the
construction of nested landscapes' hierarchies.
*/
trait TopLandscape extends Landscape :
  val scale: Double

object TopLandscape :
  def numberOfUnits(scale: Double, basesize: Int): Int = (basesize * scale).toInt
end TopLandscape

/**
This is a trait that can be extended by landscapes. Landscapes with spatial
stochastic events are furnished with the selectVId function that allows the
selection of the unit where the next event is going to occur given a random number
and the propensity of the event in each unit.
*/
trait SpatialStochasticEvents:

  def selectVId(
    x_rnd: Double,
    prob: ListMap[VertexId,Double]):
    VertexId =
      prob.find(x_rnd <= _._2)._1

/**
This is an enumeration of land cover types considered for the biophyisical submodel
in the ecological landscape
*/
enum LandCover:
  case Natural
  case Degraded
  case LowIntAgriculture
  case HighIntAgriculture

/**
This is an enumeration of the management strategies parametrized by their spatial
clustering parameter. This parameter is fixed to 3.0 and yielding quite aggregated
patterns that make evident the spatial agency difference of the two strategies.
Land sharing prefers being close with non-agricultural land leading to dispersion,
but in some way "clustering" with non-agricultural. Land sparing prefers clustering
agricultural land together.
*/
enum MngStrategy(clustering: Double):
  case LandSharing(clustering = 3.0)
  case LandSparing(clustering = 3.0)

/**
hierarchical nested enums for the event types
*/
enum EventType:
  case SpontaneousEvent
  case ConversionEvent
  case DemographicEvent:

enum SpontaneousEvent:
  case Recovery
  case Degradation
  case FertilityLossEvent

enum FertilityLossEvent:
  case LowIntensityFertilityLoss
  case HighIntensityFertilityLoss

enum ConversionEvent:
  case LowIntensityConversion
  case HighIntensityConversion

enum DemographicEvent:
  case Birth
  case Death
