case class ManagementUnit(id: Int, composition: Set[Int], strategy: String){

  def id = this.id
  def composition = this.composition
  def strategy = this.strategy

  def isAvailable(landscape: PlanningLandscape): Bool =
    this.composition.exists{ landscape.available.contains(_) }

  def conversionPropensity(landscape: PlanningLandscape, total_propensity: Double): ParMap[Int,Double] =
    landscape.conversionWeights(this.composition, this.strategy).map(_ * total_propensity).toMap.par

}

object ManagementUnit{


}
