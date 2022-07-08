
/**
* A World is defined by its time, its biophysical, planning and management
* landscapes, its populations and its parameters, which are the world's universal
* constants.
*/
case class World(t: Double,
                 eco: Graph[EcoUnit,Long],
                 pln: Graph[PlanningUnit,Long],
                 mng: Graph[ManagementUnit,Long],
                 pop: Double,
                 args: Parameters){
  /**
  * @param t is the current time in the simulation
  * @return a boolean determining whether the simulation should stop or not
  */
  def hasNext(t: Double): Boolean = {
    val pred_time: Bool = t > this.args.maxT
    val pred_pop: Bool = this.pop == 0
    val pred_nat: Bool = this.eco.countNatural() == 0
    val pred_deg: Bool = this.eco.countNatural() == this.args.n * this.args.n
    !(pred_time || pred_pop || pred_nat || pred_deg)
  }

  def updated(pop: (Double,Double),
              rec: (VertexRDD[Double],Double),
              deg: (VertexRDD[Double],Double),
              flo: (VertexRDD[Double],Double),
              tmng: Double): World = {
    World.updated(pop,rec,deg,flo,tmng,this.world)
  }

}

object World{

  def apply(args: Parameters): World = {

    val eco_empty: Graph[EcoUnit,Long] = EcoLandscape.build(args.r,args.ecr)

    val npu: Int =
    val pln: Graph[PlnUnit,Long] = PlnLandscape.build(npu,args.r)

    val nmu: Int =
    val mng: Graph[MngUnit,Long] = MngLandscape.build(nmu,npu,args.fs)

    val eco: Graph[EcoUnit,Long] =
      EcoLandscape.initialize(eco_empty,pln,mng,args.size,args.z,args.fagr,args.fdeg)

    val res: Double =
      PlnLandscape.totalResources(pln,eco,args.y_es,args.z,args.his)
    val pop: Double = HumanPopulation.build(res)

    World(eco,pln,mng,pop,args)
  }

  /**
  * @param world is this world
  * @param eco is the ecological landscape
  * @return a world with an updated landscape
  */
  def updatedEco(world: World,
                 eco: Graph[EcoUnit,Long]): World = {
    world.copy(eco,world.pln,world.mng,world.pop,world.args)
  }

  /**
  * @param world is this world
  * @param eco is the ecological landscape
  * @return a world with an updated population
  */
  def updatedPop(world: World,
                 pop: Double): World = {
    world.copy(world.eco,world.pln,world.mng,pop,world.args)
  }

  /**
  * @param pop is the human population propensity
  * @param rec is the recovery propensity
  * @param deg is the degradation propensity
  * @param flo is the fertility loss propensity
  * @param tmng is the total management propensity
  * @return the updated world given the propensities
  */
  def updated(pop: (Double,Double),
              rec: (VertexRDD[Double],Double),
              deg: (VertexRDD[Double],Double),
              flo: (VertexRDD[Double],Double),
              tmng: Double,
              world: World): World = {

    val tpop: Double = pop._1 + pop._2
    val tspont: Double = rec._2 + deg._2 + flo._2

    val x_rand: Double = rnd.nextDouble(tpop+tspont+tmng)

    selectEventGeneralType(x_rand, tspont, tmng, tpop) match {
      case "Population" => {
        val upd_pop: Int = applyPopulationEvent(x_rand,pop,world.pop)
        updatedPop(world,upd_pop)
      }
      case "Spontaneous" => {
        selectSpontaneous(x_rand,(rec._2,deg._2,flo._2)) match {
          case "Recovery" => {
            val upd_eco: Graph[EcoUnit,Long] = applySpontaneousEvent(x_rand,rec._1,eco,"Natural")
          }
          case "Degradation" => {
            val upd_eco: Graph[EcoUnit,Long] = applySpontaneousEvent(x_rand,deg._1,eco,"Degraded")
          }
          case "FertilityLoss" => {
            val upd_eco: Graph[EcoUnit,Long] = applySpontaneousEvent(x_rand,deg._1,eco,"Degraded")
          }
        }
        updatedEco(world,upd_eco)
      }
      case "Management" => {
        val upd_eco: Graph[EcoUnit,Long] = applyConversionEvent(x_rand,world.eco,world.pln,world.mng,mngt)
        updatedEco(world,upd_eco)
      }
    }
  }

  def applyPopulationEvent(x_rand: Double,
                           prop: (Double,Double),
                           pop: Double): Double {
    selectBirthOrDeath(x_rand,pop) match {
      case "Birth" => pop + 1.0
      case "Death" => pop - 1.0
    }
  }
  def applySpontaneousEvent(x_rand: Double,
                            prop: VertexRDD[Double],
                            eco: Graph[EcoUnit,Long],
                            cover: String): Graph[EcoUnit,Long] = {
    val vid: VertexId = S3Utils.selectVId(x_rand,prop)
    EcoLandscape.updated(vid,cover,eco)
  }
  def applyConversionEvent(x_rand: Double,
                           eco: Graph[EcoUnit,Long],
                           pln: Graph[PlnUnit,Long],
                           mng: Graph[MngUnit,Long],
                           mngt: Double): Graph[EcoUnit,Long] = {
    val mngp: VertexRDD[Double] =
     ManagementLandscape.conversionPropensity(mng,pln,eco,mngt)
    val mid: VertexId =
     S3Utils.selectVId(rnd_x,mngp)
    val plnp: VertexRDD[Double] =
     mng.lookup(mid).conversionPropensity(pln,eco,mngt)
    val pid: VertexId =
     S3Utils.selectVId(rnd_x,plnp)
    val vids: VertexRDD[VertexId] = pln.lookup(pid)
    mng.lookup(mid).stg match{
     case "Sharing" => EcoLandscape.updated(vids, "Low-intensity", eco)
     case "Sparing" => EcoLandscape.updated(vids, "High-intensity", eco)
    }
  }

  /**
  * @param x_rand is the random number thrown to sample the distributions
  * @param spont is the total spontaneous propensity + the population one
  * @param mng is the total management propensity + spontaneous + population
  * @param pop is the total population propensity
  * @return a string identifying the general type of event
  */
  def selectEventGeneralType(x_rand: Double,
                             spont: Double,
                             mng: Double,
                             pop: Double): String = {
    x_rand match {
      case x if x < pop => "Population"
      case x if x < spont => "Spontaneous"
      case x if x < mng => "Management"
      case other => println(s"Cannot select a transition, propensity upper bound in the selector is larger than expected.")
    }
  }

  /**
  * @param x_rand is the random number thrown to sample the distributions
  * @param prop contains the birth and death propensities in field 1 and 2 respectively
  * @return a string with the type of human population event
  */
  def selectBirthOrDeath(x_rand: Double,
                         prop: (Double, Double)): String = {
    x_rand match {
      case x if x < prop._1 => "Birth"
      case x if x < prop._2 => "Death"
      case other => println(s"selectBirthOrDeath: Cannot select a transition, propensity upper bound in the selector is larger than expected.")
    }
  }

  /**
  * @param x_rand is the random number thrown to sample the distributions
  * @param prop contains the recovery, degradation and fertility loss propensities in field 1,2 and 3 respectively
  * @return a string with the type of spontaneous land cover transition
  */
  def selectSpontaneous(x_rand: Double,
                        prop: (Double,Double,Double)): String = {
    x_rand match {
      case x if x < prop._1 => "Recovery"
      case x if x < prop._2 => "Degradation"
      case x if x < prop._3 => "FertilityLoss"
      case other => println(s"selectSpontaneous: Cannot select a transition, propensity upper bound in the selector is larger than expected.")
  }
}
// val parameters = InputOutput.parseArgs(args)
