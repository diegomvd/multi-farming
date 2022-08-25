/**
The human population is implemented in case class and companion object HumanPop.
Human population is described by its size and its sensitivity to resource deficit
which controls the celerity of human response to a resource deficit.
Note: the total conversion propensity is calculated from the HumanPop case class.
@author diego
TODO: Still need to write the function for the death propensity
*/

case class HumanPop(
  size: Int,
  sres: Double):

  /**
  @param demo is the type of demographic event: a birth or death
  @return a human population with an updated size and identical sensitivity to
          resource deficit
  */
  def update(demo: EventType): HumanPop =
      demo match {
        case Birth => this.copy(size = HumanPop.birth(this.size))
        case Death => this.copy(size = HumanPop.death(this.size))
      }

  /**
  @return the resource demand of the human population given its size and available
          resource
  */
  def resourceDemand(resources: Double): Double =
    this.size.toDouble - resources

  /**
  @return the total conversion propensity given the resource demand and the
          sensitivity to resource demand
  */
  def totalConversionPropensity(resources: Double): Double =
    this.sres * this.resourceDemand(resources)

  /**
  @param ival is the initial value for the cummulative sum of the propensities
  @return a tuple with birth and death propensities in first and second positions respectively
  */
  def demographicPropensities(
    ival: Double,
    resources: Double):
    (Double,Double) =
      val birth: Double = HumanPop.birthPropensity(ival)
      val death: Double = HumanPop.deathPropensity(birth,this.size,resources)
      (birth,death)

object HumanPop :

  def apply(resources: Double, sres: Double): Int = HumanPop(resources.toInt,sres)
  def birth(size: Int): Int = size + 1
  def death(size: Int): Int = size - 1
  def birthPropensity(ival: Double): Double = ival + 1.0
  def deathPropensity(
    ival: Double,
    size: Int,
    resources: Double): Double =
      ival + 0.0 //function here

end HumanPop
