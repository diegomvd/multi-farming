/**
This object provides all the functions needed to interact with a human population
represented by its size
1- Build and update the population
2- Calculate demand for resources
3- Calculate propensities for birth and death events
*/

object HumanPop{

  def build(resources: Double): Int = resources.toInt

  def birth(size: Int): Double = size + 1
  def death(size: Int): Double = size - 1

  def updated(size: Int,
              demo: String): Int = {
    demo match {
      case "Birth" => birth(size)
      case "Death" => death(size)
    }
  }

  def resourceDemand(size: Int, resources: Double): Double {
    size.toDouble - resources
  }

  def birthPropensity(ival: Double): Double = ival + 1.0
  def deathPropensity(ival: Double,
                      size: Int,
                      resources: Double): Double = {
    ival + //function here
  }
  /**
  @return a tuple with birth and death propensities in first and second positions respectively
  */
  def propensities(ival: Double,
                   size: Int,
                   resources: Double): (Double,Double) = {
    val birth: Double = birthPropensity(ival)
    val death: Double = deathPropensity(birth,size,resources)
    (birth,death)
  }

}
