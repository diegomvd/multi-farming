case class PlanningUnit(id: Int, composition: Set[ModuloCoord], neighbors: Vector[Int], eco_neighbors: Vector[ModuloCoord]){

  def id = this.id
  def composition = this.composition
  def neighbors = this.neighbors
  def ecoNeighbors = this.eco_neighbors

  def isAvailable(landscape: BioPhyLandscape): Bool =
    PlanningUnit.isAvailable(this.composition, landscape.composition)

  def conversionWeight(landscape: BioPhyLandscape, strategy: String){
    if this.isAvailable {
      strategy match {
        case "Sparing" => {
          val non_natural_neighbors = this.eco_neighbors.collect{
            landscape.composition.get(_).isNotNatural
          }.toVector.size
          // here the function to determine the propensity depending on neighborhood
        }

        case "Sharing" => {
          val natural_neighbors = this.eco_neighbors.filter{
            landscape.composition.get(_).isNatural
          }.toVector.size
          // here the function to determine the propensity depending on neighborhood
        }
      }
    }
    else 0.0
  }

}

object PlanningUnit {

  /**
  This function recovers the EcoUnits in the manhattan neighborhood of each
  EcoUnit in this with threshold = 1. As such it is only use as adjacency in a
  Voronoi tesselation process to determine the higher-level strategic units.
  **/
  def ecoNeighbors(radius: Int, unit_composition: Set[ModuloCoord]) = Vector[ModuloCoord]{
    unit_composition.map( _.manhattanNeighbors(radius,1) ).toVector
  }

  def neighbors(radius: Int,  unit_id: Int, landscape: ParMap[Int, PlanningUnit]) = Vector[PlanningUnit] {
    PlanningUnit.ecoNeighbors(radius, landscape.get(unit_id).composition).map{ en => landscape.find( _._2.composition.exists.(_.contains(en)))._2 }.toSet.toVector
  }

  def isAvailable(unit_composition: Set[ModuloCoord], biophy_composition: ParMap[ModuloCoord, EcoUnit] ){
    unit_composition.forall{ biophy_composition.get(_).isNotCultivated() }
  }

  def

}
