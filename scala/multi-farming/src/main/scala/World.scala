
/**
* A World is defined by its time, its biophysical, planning and management
* landscapes, its populations and its parameters, which are the world's universal
* constants.
*/
case class World(t: Double,
                 eco: Graph[EcoUnit,Long],
                 pln: Graph[PlanningUnit,Long],
                 mng: Graph[ManagementUnit,Long],
                 pop: Int,
                 args: Parameters){
  /**
  @param t is the current time in the simulation
  @return a boolean determining whether the simulation should stop or not
  */
  def hasNext(): Boolean = {
    val pred_time: Bool = this.t > this.args.maxT
    val pred_pop: Bool = this.pop == 0
    val pred_nat: Bool = this.eco.countNatural() == 0
    val pred_deg: Bool = this.eco.countNatural() == this.args.size
    !(pred_time || pred_pop || pred_nat || pred_deg)
  }

  def updated(pop: (Double,Double),
              spont: ((ListMap[VertexId,Double],ListMap[VertexId,Double],ListMap[VertexId,Double],ListMap[VertexId,Double]),Double),
              tcp: Double): World = {
    World.updated(pop,spont,tcp,this)
  }

}

object World{

  def apply(args: Parameters): World = {

    val eco_pristine: Graph[EcoUnit,Long] = EcoLandscape.build(args.r,args.ecr)
    val npu: Int = args.pscale/args.size
    val pln: Graph[PlnUnit,Long] = PlnLandscape.build(npu,args.r)
    val nmu: Int = args.mscale/args.size
    val mng: Graph[MngUnit,Long] = MngLandscape.build(nmu,npu,args.fs)

    val eco: Graph[EcoUnit,Long] =
      EcoLandscape.init(eco_pristine,pln,mng,args.size,args.z,args.fagr,args.fdeg)

    val res: Double =
      EcoLandscape.resources(eco,args.z,args.size,args.y_es,args.his)
    val pop: Int = HumanPop.build(res)

    World(0.0,eco,pln,mng,pop,args)
  }

  /**
  @param world is this world
  @param eco is the ecological landscape
  @return a world with an updated biophyscal landscape
  */
  def updatedEco(world: World,
                 new_t: Double,
                 eco: Graph[EcoUnit,Long]): World = {
    world.copy(new_t,eco,world.pln,world.mng,world.pop,world.args)
  }

  /**
  @param world is this world
  @param eco is the ecological landscape
  @return a world with an updated population
  */
  def updatedPop(world: World,
                 new_t: Double,
                 pop: Double): World = {
    world.copy(new_t,world.eco,world.pln,world.mng,pop,world.args)
  }

  /**
  @param pop is the human population propensity
  @param rec is the recovery propensity
  @param deg is the degradation propensity
  @param flo is the fertility loss propensity
  @param tcp is the total conversion propensity
  @return the updated world given the propensities
  */
  def updated(pop: (Double,Double),
              spont: ((ListMap[VertexId,Double],ListMap[VertexId,Double],ListMap[VertexId,Double],ListMap[VertexId,Double]),Double),
              tcp: Double,
              world: World): World = {

    val new_t: Double = world.t - 1/log(rnd.nextDouble(pop._2 + spont._5 + tcp))
    // random number to select an event, maximum is the sum of the cumulative propensities
    val x_rnd: Double = rnd.nextDouble(pop._2 + spont._5 + tcp)

    selectEventGeneralType(x_rnd,pop._2,spont._2,tcp) match {
      case "Population" => {
        val upd_pop: Int = applyPopulationEvent(x_rnd,pop,world.pop)
        updatedPop(world,new_t,upd_pop)
      }
      case "Spontaneous" => {
        selectSpontaneous(x_rnd,spont._1) match {
          case "Recovery" => {
            val upd_eco: Graph[EcoUnit,Long] = applySpontaneousEvent(x_rnd,spont._1._1,eco,"Natural")
          }
          case "Degradation" => {
            val upd_eco: Graph[EcoUnit,Long] = applySpontaneousEvent(x_rnd,spont._1._2,eco,"Degraded")
          }
          case "FertilityLossLow" => {
            val upd_eco: Graph[EcoUnit,Long] = applySpontaneousEvent(x_rnd,spont._1._3,eco,"Natural")
          }
          case "FertilityLossHigh" => {
            val upd_eco: Graph[EcoUnit,Long] = applySpontaneousEvent(x_rnd,spont._1._4,eco,"Degraded")
          }
        }
        updatedEco(world,new_t,upd_eco)
      }
      case "Conversion" => {
        val upd_eco: Graph[EcoUnit,Long] = applyConversionEvent(x_rnd,spont._2,world.eco,world.pln,world.mng,tcp)
        updatedEco(world,new_t,upd_eco)
      }
    }
  }

  def applyPopulationEvent(x_rnd: Double,
                           prop: (Double,Double),
                           pop: Double): Double {
    selectBirthOrDeath(x_rnd,prop) match {
      case "Birth" => pop + 1.0
      case "Death" => pop - 1.0
    }
  }

  def applySpontaneousEvent(x_rnd: Double,
                            prop: ListMap[VertexId,Double],
                            eco: Graph[EcoUnit,Long],
                            cover: String): Graph[EcoUnit,Long] = {
    val vid: VertexId = S3Utils.selectVId(x_rnd,prop)
    EcoLandscape.updated(vid,cover,eco)
  }

  def applyConversionEvent(x_rnd: Double,
                           ival: Double,
                           eco: Graph[EcoUnit,Long],
                           pln: Graph[PlnUnit,Long],
                           mng: Graph[MngUnit,Long],
                           tcp: Double): Graph[EcoUnit,Long] = {
    val mngp: ListMap[VertexId,Double] =
     MngLandscape.propensities(ival,tcp,mng,pln,eco)
    val vid: VertexId =
     S3Utils.selectVId(rnd_x,mngp)

    val max: Double = mngp.get(vid)
    // this approach works because every management unit can be selected with
    // unifrom probability. Else it is needed to access the previous element
    val utcp: Double = mngp.head._2
    val ival2: Double = max-step
    val plnp: VertexRDD[Double] =
     mng.lookup(mid).propensities(ival2,step,pln,eco)
    val pid: VertexId =
     S3Utils.selectVId(rnd_x,plnp)

    val vids: VertexRDD[VertexId] = pln.lookup(pid)
    mng.lookup(mid).stg match{
     case "Sharing" => EcoLandscape.updated(vids, "Low-intensity", eco)
     case "Sparing" => EcoLandscape.updated(vids, "High-intensity", eco)
    }
  }

  /**
  @param x_rnd is the random number thrown to sample the distributions
  @param spont is the total spontaneous propensity + the population one
  @param mng is the total management propensity + spontaneous + population
  @param pop is the total population propensity
  @return a string identifying the general type of event
  */
  def selectEventGeneralType(x_rnd: Double,
                             pop: Double,
                             spont: Double,
                             tcp: Double): String = {
    x_rnd match {
      case x if x < pop => "Population"
      case x if x < spont => "Spontaneous"
      case x if x < tcp => "Conversion"
      case other => println(s"Cannot select a transition, propensity upper bound in the selector is larger than expected.")
    }
  }

  /**
  @param x_rnd is the random number thrown to sample the distributions
  @param prop contains the birth and death propensities in field 1 and 2 respectively
  @return a string with the type of human population event
  */
  def selectBirthOrDeath(x_rnd: Double,
                         prop: (Double, Double)): String = {
    x_rnd match {
      case x if x < prop._1 => "Birth"
      case x if x < prop._2 => "Death"
      case other => println(s"selectBirthOrDeath: Cannot select a transition, propensity upper bound in the selector is larger than expected.")
    }
  }

  /**
  @paramx_rnd is the random number thrown to sample the distributions
  @paramprop contains the recovery, degradation and fertility loss propensities in field 1,2 and 3 respectively
  @return a string with the type of spontaneous land cover transition
  */
  def selectSpontaneous(x_rnd: Double,
                        prop: (Double,Double,Double,Double)): String = {
    x_rnd match {
      case x if x < prop._1 => "Recovery"
      case x if x < prop._2 => "Degradation"
      case x if x < prop._3 => "FertilityLoss"
      case other => println(s"selectSpontaneous: Cannot select a transition, propensity upper bound in the selector is larger than expected.")
  }
}
// val parameters = InputOutput.parseArgs(args)
