@main def hello: Unit =
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"

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

object Main{

  val params = InputOutput.parseArgs()
  var world: World = World.apply(params)
  world.run()

}

object Simulation{

  /**
  * The function runs the simulation in a recursive way
  */
  def run(args): World = {

    // this is the initialization
    val world = World.build(args)

    val es_flow = EcoLandscape.esFlow(world.eco,args.size, args.z) // ncc are calculated here, this is not the optimal if I want to save ncc data instead of recalculating at the time of the analysis
    val resources = PlanningLandscape.resources() //
    val pop_prop =
    val spont_prop =
    val mng_prop =

    var t_save = args.saveTime
    // here is the recursive function to execute the simulation
    @tailrec
    def rec(world: World, args: Parameters, ): World = {

      // saving data
      if(t > t_save){
        UtilsIO.save(world,ncc,es,resources)
        t_save = t + t_save
      }

      if world.doesNotHaveNext(....){
        world
      }
      else{
        // selecting next time, this throws a random number inside
        val t: Double = world.nextTime(...)

        // updating the world given the propensities
        val x_rand: Double = rnd.nextDouble(sumprop)
        val (upd_world, upd_event) = world.updated(...)

        upd_event match {
          case ("Recovery" || "Degradation" || "Conversion") => {
            val upd_es = EcoLandscape.esFlow(upd_world.eco, total_area, args.z)
            val upd_resources = PlanningLandscape.resources()
            val upd_pop_prop =
            val upd_spont_prop =
            val upd_mng_prop =
          }
          case "FertilityLoss" => {
            val upd_es = es
            val upd_resources =
            val upd_pop_prop =
            val upd_spont_prop =
            val upd_mng_prop =
          }
          case "Population" => {
            val upd_es = es
            val upd_resources = resources
            val upd_pop_prop =
            val upd_spont_prop = spont_prop
            val upd_mng_prop =
          }
        }
        rec(...)
      }
    }
  }

}






// write the InputOutput class to define the outputs that are to be used by OM
// check what OM needs, if scala variables or files...
