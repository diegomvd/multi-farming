package model
/**
Implementation of the Ecological Units. EcoUnits are the elementary constituents of an EcoLandscape and are defined by
their land cover.
@author diego
@todo write the expression of the production in low-intensity EcoUnits.
*/

case class EcoUnit(cover: LandCover):
  /**
   * @param c is the land cover to match against this EcoUnit's cover
   * @return true if there's a match, false if there's not
   * */
  def matchCover(c: LandCover): Boolean =
    EcoUnit.matchCover(this.cover,c)

object EcoUnit:

  /**
  @param c1 first land cover type
  @param c2 second land cover type
  @return true if both covers are equal, false if not
  */
  def matchCover(c1: LandCover, c2: LandCover): Boolean =  c1 == c2

  /**
  @param s is the sensitivity to ecosystem service inflow
  @param es is the ecosystem service inflow
  @return the recovery/degradation propensity
  */
  def increasingPES(s: Double, es: Double): Double =  s*es
  def decreasingPES(s: Double, es: Double): Double =  (1-es)*s

  /**
  @param y1 is
  @param y2 is the contribution of ecosystem services to resource production
  @param es is the ecosystem service inflow
  @return the potential resource production in this ecological unit
  */
  def lowIntResEquation(
    y1: Double,
    y2: Double,
    es: Double): Double = 0.0 // TODO: fill the function


  def highIntResEquation(): Double = 1.0

end EcoUnit

