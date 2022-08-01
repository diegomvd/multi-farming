import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph

trait Landscape:

  val composition: Graph[A, Long]
  val size: Int

  def updateComposition(a: A, b: A): Graph[A, Long] =
    composition.mapValues(case a => b)

  def updateComposition(a: VertexRDD[A], b: A): Graph[A, Long] =
    composition.mapValues{ case (vid,attr) if a.contains((vid,attr)) => b }

trait BaseLandscape extends Landscape with VoronoiTesselation

trait TopLandscape extends Landscape :
  val scale: Double

object TopLandscape :
  def numberOfUnits(scale: Double, basesize: Int):Int = (basesize * scale).toInt
end TopLandscape

trait SpatialStochasticEvents:

  def selectVId(
    x_rnd: Double,
    prob: ListMap[VertexId,Double]):
    VertexId =
      prob.find(x_rnd <= _._2)._1

enum LandCover:
  case Natural
  case Degraded
  case LowIntAgriculture
  case HighIntAgriculture

enum MngStrategy(clustering: Double)
  case LandSharing(clustering = 3.0)
  case LandSparing(clustering = 3.0)

enum EventType:
  case Spontaneous:
    case Recovery
    case Degradation
    case FertilityLoss:
      case LowIntensityFertilityLoss
      case HighIntensityFertilityLoss
  case Conversion:
    case LowIntensityConversion
    case HighIntensityConversion
  case Demographic:
    case Birth
    case Death
