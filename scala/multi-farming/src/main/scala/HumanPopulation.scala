case class HumanPopulation(size: Int){

  // methods to update population size
  def birth() = HumanPopulation { HumanPopulation(this.size+1) }
  def death() = HumanPopulation { HumanPopulation(this.size-1) }

  def birthPropensity() = Double { // function here
  }

  def deathPropensity(carrying_capacity: Double) = Double {
    //function here
  }

}
