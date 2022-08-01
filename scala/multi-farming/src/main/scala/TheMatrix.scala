
/**
* A World is defined by its time, its biophysical, planning and management
* landscapes, its populations and its parameters, which are the world's universal
* constants.
*/
case class TheMatrix(
  t: Double,
  maxTime: Double,
  eco: EcoLandscape,
  pln: PlnLandscape,
  mng: MngLandscape,
  pop: HumanPop):

  /**
  This functions terminates execution of the simulation if:
   -time exceeds maximum
   -human population size reaches 0
   -number of natural cells reaches 0
   -landscape is pristine and population size 0
  @return a boolean determining whether the simulation should stop or not
  */
  def doesNotHaveNext: Boolean =
    val pred_time: Bool = this.t > this.args.maxT
    val pred_pop: Bool = this.pop == 0
    val pred_deg: Bool = this.eco.countNatural() == 0
    val pred_nat: Bool = this.eco.countNatural() == this.args.size
    (pred_time || pred_pop || pred_deg || (pred_nat && pred_pop))

  /**
  This function updates the world given the events' propensities
  @return an updated world
  */
  def update(
    pop: (Double,Double),
    spont: ((ListMap[VertexId,Double],ListMap[VertexId,Double],ListMap[VertexId,Double],ListMap[VertexId,Double]),Double),
    tcp: Double):
    (TheMatrix,EventType) =
      TheMatrix.update(pop,spont,tcp,this)

  def initialize()

  /**
  This function runs the world's dynamics:
  1- Get the natural connected components
  2- Get the ecosystem services flow
  3- Get resource production
  4- Calculate propensities
  5- Update the world
  @return the state of the world
  TODO: ecosystemServiceFlow does not return a joined graph and that's needed,
  some adjustements to be done
  */
  def run: TheMatrix = {

    val (ncc, es) = this.eco.ecosystemServiceFlow
    val res = this.eco.resourceProduction(es)
    val popp = this.pop.propensities(0.0,res)
    val spontp = this.eco.updatePropensities(popp._2,es)
    val tcp = this.pop.totalConversionPropensity(res)

    @annotation.tailrec
    def rec(world: TheMatrix,
            ncc: VertexRDD[VertexId],
            es: Graph[(EcoUnit,Double),Long],
            res: Double,
            popp: (Double,Double),
            spontp: ((ListMap[VertexId,Double],ListMap[VertexId,Double],ListMap[VertexId,Double],ListMap[VertexId,Double]),Double),
            tcp: Double): TheMatrix = {
      if world.doesNotHaveNext { world }
      else {
        val new_world: (TheMatrix, EventType) = world.updated(popp,spontp,tcp)
        new_world._2 match {
          case Demographic => {
            val new_ncc = ncc
            val new_es = es
            val new_res = res
            val new_popp = new_world._1.pop.propensities(0.0,new_res)
            val new_spontp = spontp
            val new_tcp = new_world._1.pop.totalConversionPropensity(new_res)
          }
          case HighIntensityFertilityLoss => {
            val new_ncc = ncc
            val new_es = es
            val new_res = res - 1.0 // this is just loosing one high intensity unit
            val new_popp = new_world._1.pop.propensities(0.0,new_res)
            val new_spontp = new_world._1.eco.updatePropensities(new_popp._2,new_es)
            val new_tcp = new_world._1.pop.totalConversionPropensity(new_res)
          }
          case other => {
            val (new_ncc, new_es) = new_world._1.eco.ecosystemServiceFlow
            val new_res = new_world._1.eco.resourceProduction(es)
            val new_popp = new_world._1.pop.propensities(0.0,res)
            val new_spontp = new_world._1.eco.updatePropensities(popp._2,es)
            val new_tcp = new_world._1.pop.totalConversionPropensity(res)
          }
        }
        rec(new_world._1,new_ncc,new_es,new_res,new_popp,new_spontp,new_tcp)
      }
    }
    rec(this,ncc,es,res,popp,spontp,tcp)
  }

}

object TheMatrix :

  /**
  TODO: this function requires changes, specially to determine the way into which
  the simulation should be initialized.
  */
  def apply(args: Parameters): TheMatrix = {

    val eco_pristine: Graph[EcoUnit,Long] = EcoLandscape.natural(args.r,args.ecr)
    val npu: Int = args.pscale/args.size
    val pln: Graph[PlnUnit,Long] = PlnLandscape.build(npu,args.r)
    val nmu: Int = args.mscale/args.size
    val mng: Graph[MngUnit,Long] = MngLandscape.build(nmu,npu,args.fs)

    val eco: Graph[EcoUnit,Long] =
      EcoLandscape.init(eco_pristine,pln,mng,args.size,args.z,args.fagr,args.fdeg)

    val res: Double =
      EcoLandscape.resources(eco,args.z,args.size,args.y_es,args.his)
    val pop: Int = HumanPop.build(res)

    TheMatrix(0.0,eco,pln,mng,pop,args)
  }

  /**
  @param world is this world
  @param eco is the ecological landscape
  @return a world with an updated biophyscal landscape
  */
  def updateEco(
    world: TheMatrix,
    new_t: Double,
    new_eco: EcoLandscape):
    TheMatrix =
      world.copy(t = new_t, eco = new_eco)

  /**
  @param world is this world
  @param eco is the ecological landscape
  @return a world with an updated population
  */
  def updatePop(
    world: TheMatrix,
    new_t: Double,
    new_pop: Double):
    TheMatrix =
      world.copy(t = new_t, pop = new_pop)


  /**
  @param pop is the human population propensity
  @param spont are the spontaneous propensities
  @param tcp is the total conversion propensity
  @return the updated world given the propensities
  */
  def update(
    pop: (Double,Double),
    spont: ((ListMap[VertexId,Double],ListMap[VertexId,Double],ListMap[VertexId,Double],ListMap[VertexId,Double]),Double),
    tcp: Double,
    world: TheMatrix):
    (TheMatrix, EventType) = {

    val new_t: Double = world.t - 1/log(rnd.nextDouble(pop._2 + spont._2 + tcp))
    // random number to select an event, maximum is the sum of the cumulative propensities
    val x_rnd: Double = rnd.nextDouble(pop._2 + spont._2 + tcp)

    selectEventType(x_rnd,pop._2,spont._2,tcp) match {
      case Demographic => {
        val upd_pop: Int = applyPopulationEvent(x_rnd,pop,world.pop)
        (updatePop(world,new_t,upd_pop),Population)
      }
      case Spontaneous => {
        selectSpontaneous(x_rnd,spont._1) match {
          case Recovery => {
            val upd_eco: Graph[EcoUnit,Long] = applySpontaneousEvent(x_rnd,spont._1._1,eco,Natural)
            val event = Recovery
          }
          case Degradation => {
            val upd_eco: Graph[EcoUnit,Long] = applySpontaneousEvent(x_rnd,spont._1._2,eco,Degraded)
            val event = Degraded
          }
          case LowIntensityFertilityLoss => {
            val upd_eco: Graph[EcoUnit,Long] = applySpontaneousEvent(x_rnd,spont._1._3,eco,Natural)
            val event = LowIntensityFertilityLoss
          }
          case HighIntensityFertilityLoss => {
            val upd_eco: Graph[EcoUnit,Long] = applySpontaneousEvent(x_rnd,spont._1._4,eco,Degraded)
            val event = HighIntensityFertilityLoss
          }
        }
        (updateEco(world,new_t,upd_eco),event)
      }
      case Conversion => {
        val upd_eco: Graph[EcoUnit,Long] = applyConversionEvent(x_rnd,spont._2,world.eco,world.pln,world.mng,tcp)
        (updateEco(world,new_t,upd_eco),Conversion)
      }
    }
  }

  /**
  TODO: all the functions below require examination to correct and determine where they belong
  */
  def applyPopulationEvent(
    x_rnd: Double,
    prop: (Double,Double),
    pop: HumanPop): HumanPop =
      pop.update(selectBirthOrDeath(x_rnd,prop))

  def applySpontaneousEvent(
    x_rnd: Double,
    prop: ListMap[VertexId,Double],
    eco: EcoLandscape,
    cover: LandCover): EcoLandscape =
      val vid: VertexId = eco.selectVId(x_rnd,prop)
      eco.update(vid,cover)

  def applyConversionEvent(
    x_rnd: Double,
    ival: Double,
    eco: Graph[EcoUnit,Long],
    pln: Graph[PlnUnit,Long],
    mng: Graph[MngUnit,Long],
    tcp: Double):
    Graph[EcoUnit,Long] = {
      val mngp: ListMap[VertexId,Double] =
       MngLandscape.propensities(ival,tcp,mng,pln,eco)
      val vid: VertexId =
       StochSimUtils.selectVId(x_rnd,mngp)

      val max: Double = mngp.get(vid)
      // this approach works because every management unit can be selected with
      // unifrom probability. Else it is needed to access the previous element
      val utcp: Double = mngp.head._2
      val ival2: Double = max-step
      val plnp: VertexRDD[Double] =
       mng.lookup(mid).propensities(ival2,step,pln,eco)
      val pid: VertexId =
       StochSimUtils.selectVId(x_rnd,plnp)

      val vids: VertexRDD[VertexId] = pln.lookup(pid)
      mng.lookup(mid).stg match{
       case LandSharing => EcoLandscape.updated(vids, LowIntensity, eco)
       case LandSparing => EcoLandscape.updated(vids, HighIntensity, eco)
      }
  }

  /**
  @param x_rnd is the random number thrown to sample the distributions
  @param spont is the total spontaneous propensity + the population one
  @param mng is the total management propensity + spontaneous + population
  @param pop is the total population propensity
  @return a string identifying the general type of event
  */
  def selectEventType(
    x_rnd: Double,
    pop: Double,
    spont: Double,
    tcp: Double):
    EventType =
      x_rnd match {
        case x if x < pop => Demographic
        case x if x < spont => Spontaneous
        case x if x < tcp => Conversion
    }

  /**
  @param x_rnd is the random number thrown to sample the distributions
  @param prop contains the birth and death propensities in field 1 and 2 respectively
  @return a string with the type of human population event
  TODO: this should be inside the human population class
  */
  def selectBirthOrDeath(
    x_rnd: Double,
    prop: (Double, Double)):
    EventType =
      x_rnd match {
        case x if x < prop._1 => Birth
        case x if x < prop._2 => Death
      }

  /**
  @paramx_rnd is the random number thrown to sample the distributions
  @paramprop contains the recovery, degradation and fertility loss propensities in field 1,2 and 3 respectively
  @return a string with the type of spontaneous land cover transition
  TODO: this should be inside EcoLandscape
  */
  def selectSpontaneous(
    x_rnd: Double,
    prop: (Double,Double,Double,Double)):
    EventType =
      x_rnd match {
        case x if x < prop._1 => Recovery
        case x if x < prop._2 => Degradation
        case x if x < prop._3 => FertilityLoss
      }
end TheMatrix
