class Simulation(parameters: Parameters){

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
