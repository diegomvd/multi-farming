package model
/**
 * A top landscape is a landscape built on top of a base landscape given its relative scale to the base landscape. A top
 * landscape can also be a base landscape, as it is the case currently with the planning landscape. This feature allows
 * for the construction of nested landscapes' hierarchies.
 * */
trait TopLandscape[T] extends Landscape[T] :
  val scale: Double

object TopLandscape :
  def numberOfUnits(scale: Double, base_size: Int): Int = (base_size * scale).toInt
end TopLandscape

