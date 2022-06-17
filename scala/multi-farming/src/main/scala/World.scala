class World(biophy_layer: BioPhyLandscape, planning_layer: PlanningLandscape, management_layer: ManagementLandscape, population_layer: HumanPopulation){

  def parseParameters()

  private var landscape: Landscape = buildLandscape()

  private var world: World = buildWorld()

  def update(): World = {
    landscape = landscape.update
    landscape
  }

  def hasNext(): Boolean = {
    if (time > maxTime || humanPopulation.size = 0 || landscape.countNatural() == 0 || landscape.countNatural() == landscape.size*landscape.size) false
    else true
  }

}

object World{

  def buildWorld(args: ) = World {

    val parameters = InputOutput.parseArgs(args)

    val biophy_layer = buildBioPhysicalLandscape()
    val planning_layer = buildPlanningLandscape()
    val management_layer = buildManagementLandscape()
    val population_layer = buildHumanPopulation()

    World(biophy_layer, planning_layer, management_layer, population_layer)

  }

}
