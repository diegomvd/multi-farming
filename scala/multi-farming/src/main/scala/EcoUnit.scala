case class EcoUnit(cover: String){

  def matchCover(c: String): Bool = {
    EcoUnit.matchCover(this.cover,c)
  }

}

object EcoUnit{

  def apply(cover: String): EcoUnit = {
    EcoUnit(cover)
  }

  /**
  @paramc1 first land cover type
  @paramc2 second land cover type
  @return true if both covers are equal, false if not
  */
  def matchCover(c1: String, c2: String): Bool = { c1 == c2 }

  /**
  @params is the sensitivity to ecosystem service inflow
  @parames is the ecosystem service inflow
  @parambool determines whether the unit has the right cover
  @return the recovery/degradation propensity
  */
  def recoveryEquation(s: Double, es: Double): Double = {
    s*es
  }
  def degradationEquation(s: Double, es: Double): Double = {
    (1-es)*s
  }

  def relationESArea(a: Double,
                     z: Double): Double = {
    pow(area,z)
  }

  def resourceLIEquation(y1: Double,
                         y2: Double,
                         es: Double): Double = {

  }
  def resourceHIEquation(): Double = {
    1.0
  }
}
