case class ManagementUnit(id: Int, comp: Set[Int], strategy: String){

  def id = this.id
  def composition = this.comp
  def strategy = this.strategy

  def isAvailable(landscape: PlanningLandscape): Bool =
    ManagementUnit.isAvailable(this.comp,landscape)

  def conversionPropensity(landscape: PlanningLandscape,
                           biocomp: ParMap[ModuloCoord,EcoUnit],
                           tcp: Double): ParMap[Int,Double] =
    ManagementUnit.conversionPropensity(this.comp,this.strategy,landscape,biocomp,tcp)

}

object ManagementUnit{

  /**
  * @param comp is the set of planning unit ids belonging to this managament unit
  * @param landscape is the planning landscape
  * @return true if the management unit is available, false if not
  */
  def isAvailable(comp: Set[Int],
                  landscape: PlanningLandscape) = Bool {
    comp.exists{ landscape.availableUnits.contains(_) }
  }

  /**
  * @param comp is the set of planning unit ids belonging to this managament unit
  * @param strategy is the land-use management strategy of this unit
  * @param landscape is the planning landscape
  * @param biocomp is the composition of the biophysical landscape
  * @param tcp is the total conversion propensity
  * @return a map with the ids of each planning unit in this management unit associated with their conversion propensity
  */
  def conversionPropensity(comp: Set[Int],
                           strategy: String,
                           landscape: PlanningLandscape,
                           biocomp: ParMap[ModuloCoord,EcoUnit],
                           tcp: Double) = ParMap[Int,Double] {
    landscape.conversionWeights(comp, strategy, biocomp).map(_ * tcp).toMap.par
  }
}
