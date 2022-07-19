/**
EcoUnits are the key base constituants of the biophysical landscape. They are
only defined by their land cover type. In this class's companion object are
defined the functions used at the EcoUnit level to calculate spontaneous propensities
and resource production.
*/

import scala.math.pow

case class EcoUnit(cover: String){
  def matchCover(c: String): Bool = {
    EcoUnit.matchCover(this.cover,c)
  }
}

object EcoUnit{

  /**
  @param c is the land cover type of the ecological unit to be initialized
  @return an ecological unit with land cover c
  */
  def apply(c: String): EcoUnit = {
    EcoUnit(c)
  }

  /**
  @param c1 first land cover type
  @param c2 second land cover type
  @return true if both covers are equal, false if not
  */
  def matchCover(c1: String, c2: String): Bool = { c1 == c2 }

  /**
  @param s is the sensitivity to ecosystem service inflow
  @param es is the ecosystem service inflow
  @return the recovery/degradation propensity
  */
  def increasingPES(s: Double, es: Double): Double = { s*es }
  def decreasingPES(s: Double, es: Double): Double = { (1-es)*s }

  /**
  @param y1 is
  @param y2 is the contribution of ecosystel services to resource production
  @param es is the ecosystem service inflow
  @return the potential resource production in this ecological unit
  */
  def lowIntResEquation(y1: Double,
                        y2: Double,
                        es: Double): Double = {

  }
  def highIntResEquation(): Double = {
    1.0
  }
}
