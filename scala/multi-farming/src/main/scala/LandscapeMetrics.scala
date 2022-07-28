case class LandscapeMetrics(resources: Double,
                            averageESFlow: Double,
                            robustnessESFlow: Double){}

object LandscapeMetrics {

  def apply(landscape: Graph[EcoUnit,Long]): LandscapeMetrics = {

    val res = EcoLandscape.resources(landscape)
    val avg = EcoLandscape.averageESFlow(landscape,z,size)
    val rob = EcoLandscape.robustnessESFlow(avg,landscape,z,size,100)

  }
}
