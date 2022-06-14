case class NaturalFragment(composition: Vector[EcologicalUnit], es_supply: Double){

  def add(unit: EcologicalUnit, area: Int, esScalingExponent: Double) = copy(this.composition:+unit, esSupply(this.composition.size()+1, area, esScalingExponent))

  def remove(unit: EcologicalUnit, area: Int, esScalingExponent: Double): NaturalFragment = {
    copy(this.composition.filterNot(_ == unit), esSupply(this.composition.size()-1, area, esScalingExponent))
  }

  def merge(fragment: NaturalFragment, area: Int, esScalingExponent: Double ) = NaturalFragment {
    // Merge two composition vectors
    val new_composition = this.composition.appendedAll(fragment.composition)
    copy(new_composition, esSupply(new_composition.size(), area, esScalingExponent))
  }

  def contains(unit: EcologicalUnit) = Boolean {
    this.composition.exists(_ == unit)
  }
}





case class NaturalFragments(composition: ParSet[ParSet[EcologicalUnit]]){

  def updateAdd(unit: EcologicalUnit,radius: Int, threshold: Int) = NaturalFragments {
    val natural_neighbors = unit.manhattanNeighbors(radius,threshold).filter( _.cover == "Natural" )
    natural_neighbors.size() match {
      case 0 => val new_composition = this.composition.incl(ParSet(unit))
      case 1 => {
        // since there is a single neighbor, a single fragment should be collected
        val new_composition = this.composition.map( case frag if frag.exists(natural_neighbors.contains(_)) => frag.incl(unit) )
      }
      case _ => {
        // collect all the fragments containing one of unit natural neighbors
        val joint_frags = this.composition.collect(case frag if frag.exists(natural_neighbors.contains(_)) => frag)
        val new_composition = this.composition.filterNot(joint_frags.contains(_)).incl(joint_frags.flatten)
      }
    }
    copy(new_composition)
  }

  def updateRmv(unit: EcologicalUnit) = NaturalFragments {



  }

  @annotation.tailrec
  def findConnectedComponent(unit: EcologicalUnit, radius:Int, threshold: Int, fragment: ParSet[EcologicalUnit]){
    val natural_neighbor = unit.manhattanNeighbors(radius,threshold).filter( _.cover == "Natural" ).first()
    if fragment.exists( _ == natural_neighbor) fragment
    fragment.incl(natural_neighbor)

  }

  def update(op: String, unit: EcologicalUnit) = NaturalFragments {
    op match {
      case "Add" => this.updateAdd(unit) // what is the correct syntax ?
      case "Rmv" => updateRmv(unit)
    }
  }

}
