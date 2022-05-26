@main def hello: Unit =
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"


case class LandUnit(landCover : String,
                    location : loc2D,
                    findUnit:loc2D => LandUnit){
  // method to update the land cover of a land unit
  def updateCover: Option[LandUnit] = ...


}

case class Landscape(size : Int,
                     functionalConnectivity : /*Some kind of matrix*/,
                     coverMap : ParMap[Int,LandUnit],
                     managementMap : ParMap[Int,ManagementUnit],
                     fragmentVector : Vector[NaturalFragment]){
  // method to find a land unit given its position
  def findUnit(location : loc2D) : LandUnit = map(getID(location, size))
  // method to update the landscape
  def update() = Landscape(size,
                         map.map{
                           case(id,unit)=>id -> unit.update.getOrElse(unit)
                         })

}

case class NaturalFragment(compositionMap : ParMap[Int,LandUnit]){
  // method to calculate fragment area
  def fragmentArea() : Int =
  // method to update fragment composition
  def updateComposition(unit : LandUnit, operation : String) =
    case ... // how to handle fragment merge or creation ?? it needs updating fragment vector
}

case class ManagementUnit(practice : String,
                          compositionMap : ParMap[Int,LandUnit],
                          production : Double){
  // method to access management practice  is this needed? maybe I can access without a function
  def managementPractice() : String = practice

}

case class HumanPopulation(size : Int){
  // methods to update population size
  def birth() : Int = HumanPopulation(size+1)
  def death() : Int = HumanPopulation(size-1)

  def resourceDemand(management : ManagementUnit) : Double = management.production // iterate over all
}
