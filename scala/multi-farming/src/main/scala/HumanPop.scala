/**
This object provides all the functions needed to interact with a human population
represented by its size
1- Build and update the population
2- Calculate demand for resources
3- Calculate propensities for birth and death events
*/

case class HumanPop(
  size: Int
  sres: Double):

  def update(
    size: Int,
    demo: EventType):
    Int =
      demo match {
        case Birth => HumanPop.birth(size)
        case Death => HumanPop.death(size)
      }

  def resourceDemand(resources: Double): Double =
    this.size.toDouble - resources

  def totalConversionPropensity(resources: Double) =
    this.sres * this.resourceDemand(resources)  

  /**
  @return a tuple with birth and death propensities in first and second positions respectively
  */
  def propensities(
    ival: Double,
    resources: Double):
    (Double,Double) =
      val birth: Double = birthPropensity(ival)
      val death: Double = deathPropensity(birth,this.size,resources)
      (birth,death)

object HumanPop{

  def apply(resources: Double, sres: Double): Int = HumanPop(resources.toInt,sres)
  def birth(size: Int): Double = size + 1
  def death(size: Int): Double = size - 1
  def birthPropensity(ival: Double): Double = ival + 1.0
  def deathPropensity(
    ival: Double,
    size: Int,
    resources: Double): Double =
      ival + //function here

end HumanPop
