case class HumanPopulation(size: Int){

  // methods to update population size
  def birth() = HumanPopulation { HumanPopulation(this.size+1) }
  def death() = HumanPopulation { HumanPopulation(this.size-1) }

  def resourceDemand(resource_production: Double) = Double {
    this.size - resource_production
  }
  def birthPropensity(): Double = 1.0
  def deathPropensity(carrying_capacity: Double, resource_production: Double) = Double {
    //function here
  }
}

object HumanPopulation{

  def buildHumans(population_size: Double): HumanPopulation = HumanPopulation(population_size)

}
