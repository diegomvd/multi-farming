object EcoUnit{

  /**
  * @param c1 first land cover type
  * @param c2 second land cover type
  * @return true if both covers are equal, false if not
  */
  def matchCover(c1: String, c2: String) = Bool { c1 == c2 }

  /**
  * @param s is the sensitivity to ecosystem service inflow
  * @param es is the ecosystem service inflow
  * @param bool determines whether the unit has the right cover
  * @return the recovery/degradation propensity
  */
  def recoveryEquation(s: Double, es: Double, bool: Bool) = Double {
    if bool s*es
    else 0.0
  }
  def degradationEquation(s: Double, es: Double, bool: Bool) = Double {
    if bool (1-es)*s
    else 0.0
  }

  /**
  * @param f is either recovery/degradationEquation
  * @return the propensity given the chosen function
  */
  def propensity(s: Double,
                 es: Double,
                 bool: Bool,
                 f: (Double,Double,Bool) => Double) = Double {
    f(es,s,bool)
  }

}
