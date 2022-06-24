class Simulation(time: Double = 0.0,
                 params: Parameters
               )extends Iterator[World]{

  private var world: World = World.buildWorld(params)
  // private var time

  override def update(): Simulation = {
    this.world = this.world.update()

    Simulation(Simulation.newTime( this.world.totalPropensity(), params, buildWorld))
  }

  override def hasNext: Boolean = {
    if (this.time > maxTime) => false
    else => this.world.hasNext
  }

  def newTime(total_propensity: Double){
    // here the formula
  }
}

object Simulation{
  def newTime(total_propensity: Double){
    // here the formula
  }
}
