@main def hello: Unit =
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"


case class LandUnit(landCover: String,
                    location: Loc2D,
                    production: Double,
                    supplyES : Double,
                    findUnit: Loc2D => LandUnit){
  // method to update the land cover of a land unit
  def updateCover: Option[LandUnit] = ...


}

case class ManagementUnit(practice : String,
                          compositionMap : ParMap[Int,ManageableUnit],
                          production : Double){}

case class ManageableUnit(compositionMap : ParMap[Int,LandUnit]){
  def convertManageableUnit // (-> convert land units corresponding to the management unit)
}

case class Landscape(size : Int,
                     functionalConnectivity : ConnectivityMatrix,
                     coverMap : ParMap[Int,LandUnit],
                     managementMap : ParMap[Int,ManagementUnit],
                     fragmentVector : Vector[NaturalFragment]){
  // method to find a land unit given its position
  def findUnit(location : Loc2D) : LandUnit = map(getID(location, size))
  // method to update the landscape
  def update() = Landscape(size,
                         map.map{
                           case(id,unit)=>id -> unit.update.getOrElse(unit)
                         })

}

case class NaturalFragment(compositionMap : ParMap[Int,LandUnit], supplyES : Double){
  // method to calculate fragment area
  def fragmentArea() : Int =
  // method to update fragment composition
  def updateComposition(unit : LandUnit, operation : String) =
    case ... // how to handle fragment merge or creation ?? it needs updating fragment vector
}

case class HumanPopulation(size : Int){
  // methods to update population size
  def birth() : Int = HumanPopulation(size+1)
  def death() : Int = HumanPopulation(size-1)

  def resourceDemand(management : ManagementUnit) : Double = management.production // iterate over all
}

final case class UnitConnection(size: Int,
                                connectionRange: Int
                                  ){
  def matrix(size: Int, range: Int): Map[Int,Int] ={
    // create a map with the neighbours of each land unit
  }
}



class Simulation(time: Double,
                 size: Int,
                 createCell: (Loc2D, Loc2D => LandUnit) => LandUnit,
                 createLandscape: (Int, Loc2D => LandUnit) => Landscape = Landscape.create _
                 )extends Iterator[Landscape]{

  private var landscape: Landscape = createLandscape(size, createCell(_.landscape.findUnit(Loc2D)))

  override def update(): Landscape = {
    landscape = landscape.update
    landscape
  }

  override def hasNext: Boolean = {
    if (time > maxTime || humanPopulation.size = 0 || landscape.countNatural() == 0 || landscape.countNatural() == landscape.size*landscape.size) false
    else true
  }
}

val sim = Simulation()
while(sim.hasNext) sim.update
sim.find{landscape => ...}

final case class Parameters(radius:                       Int = 5,
                            maxTime:                      Double = 1,
                            agriculturalFraction:         Double = 0.2,
                            degradedFraction:             Double = 0.1,
                            strategicScale:               Int = 1,
                            managementScale:              Int = 1,
                            sparingFraction:              Double = 0.5,
                            sensitivityResourceDeficit:   Double = 1,
                            scalingExponentES:            Double = 0.2,
                            yiedlContributionES:          Double = 0.3,
                            highIntensityUnitSupport:     Double = 1, // number of househoulds supported by 1 high intensity agricultural unit
                            sensitivitySoilFertilityLoss: Double = 1,
                            sensitivityLandRecovery:      Double = 1,
                            sensitivityLandDegradation:   Double = 1,
                            savingTimeStep:               Double = 1,
                            seed:                         Long = 123456789
                          ){}

object Parameters {
  def parseParameters(params: Seq[String]): Parameters =
    if (params.size>0)
      params.flatMap(_.split('=')).sliding(2,2).foldLeft(Parameters()){
        case (acc, Seq("radius", value))                       => if(value>=1) acc.copy(radius = Integer.parseInt(value))             else println(s"parseParameters.error: trying to set size to $value but size must be equal to or larger than 1.")
        case (acc, Seq("maxTime", value))                      => if(value>=0) acc.copy(size = Real.parseDouble(value))             else println(s"parseParameters.error: trying to set maxTime to $value but maxTime cannot be negative.")
        case (acc, Seq("agriculturalFraction", value))         => if(value>=0 && value<=1) acc.copy(size = Real.parseDouble(value)) else println(s"parseParameters.error: trying to set agriculturalFraction to $value but agriculturalFraction must lay in [0-1].")
        case (acc, Seq("degradedFraction", value))             => if(value>=0 && value<=1) acc.copy(size = Real.parseDouble(value)) else println(s"parseParameters.error: trying to set degradedFraction to $value but degradedFraction must lay in [0-1].")
        case (acc, Seq("nManagementUnits", value))             => if(value>=1) acc.copy(size = Integer.parseInt(value))             else println(s"parseParameters.error: trying to set nManagementUnits to $value but nManagementUnits must be equal to or larger than 1.")
        case (acc, Seq("sparingFraction", value))              => if(value>=0 && value<=1) acc.copy(size = Real.parseDouble(value)) else println(s"parseParameters.error: trying to set sparingFraction to $value but sparingFraction must lay in [0-1].")
        case (acc, Seq("sensitivityResourceDeficit", value))   => if(value>=0) acc.copy(size = Real.parseDouble(value))             else println(s"parseParameters.error: trying to set sensitivityResourceDeficit to $value but sensitivityResourceDeficit cannot be negative.")
        case (acc, Seq("scalingExponentES", value))            => acc.copy(size = Real.parseDouble(value))                          if(value<0 || value>1)   println(s"parseParameters.error: setting scalingExponentES to $value. Caution: values outside [0-1] might cause funky behavior.")
        case (acc, Seq("yieldContributionES", value))          => if(value>=0 && value<=1) acc.copy(size = Real.parseDouble(value)) else println(s"parseParameters.error: trying to set yieldContributionES to $value but yieldContributionES must lay in [0-1].")
        case (acc, Seq("highIntensityUnitSupport", value))     => if(value>=0) acc.copy(size = Real.parseDouble(value))             else println(s"parseParameters.error: trying to set highIntensityUnitSupport to $value but highIntensityUnitSupport cannot be negative.")
        case (acc, Seq("sensitivitySoilFertilityLoss", value)) => if(value>=0) acc.copy(size = Real.parseDouble(value))             else println(s"parseParameters.error: trying to set sensitivitySoilFertilityLoss to $value but sensitivitySoilFertilityLoss cannot be negative.")
        case (acc, Seq("sensitivityLandRecovery", value))      => if(value>=0) acc.copy(size = Real.parseDouble(value))             else println(s"parseParameters.error: trying to set sensitivityLandRecovery to $value but sensitivityLandRecovery cannot be negative.")
        case (acc, Seq("sensitivityLandDegradation", value))   => if(value>=0) acc.copy(size = Real.parseDouble(value))             else println(s"parseParameters.error: trying to set sensitivityLandDegradation to $value but sensitivityLandDegradation cannot be negative.")
        case (acc, Seq("savingTimeStep", value))               => if(value>=0) acc.copy(size = Real.parseDouble(value))             else println(s"parseParameters.error: trying to set savingTimeStep to $value but savingTimeStep cannot be negative.")
        case (acc, Seq("seed", value))                         => acc.copy(size = Integer.parseInt(value).toLong)
        case (acc, Seq(name, value)) => println(s"parseParameters.error: cannot recognize parameter $name with value $value."); acc
        case (acc, _) => println(s"parseParameters.error: something went wrong"); acc
      }

}
