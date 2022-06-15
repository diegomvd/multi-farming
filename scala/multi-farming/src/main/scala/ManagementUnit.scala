case class ManagementUnit(composition: Set[EcoUnit], strategy: String){

  def manhattanNeighbors(radius: Int, threshold: Int) = Vector[ManagementUnit] {
    
  }

  def isAvailable() = Bool {
    if this.composition.exists( _.isCultivated() ) false
    else true
  }

  def conversionPropensity(resource_demand: Double) = Double {
    this.isAvailable() match {
      case true => // function here
      case false => 0
    }
  }
}
