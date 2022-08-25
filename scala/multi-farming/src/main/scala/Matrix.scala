/**
The Social-ecological, human-land system is implemented in the Matrix case class
and its companion object. A Matrix is described by its internal time, its
ecological, planning and management landscapes and its human population.
The key function is run, that starts a simulation until reaching an absorbant
state or surpassing the maximumt time specified by the user.

@author diego
*/

import scala.annotation.tailrec

case class Matrix(
  t: Double,
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
  def doesNotHaveNext(maxT: Double): Boolean =
    val pred_time: Boolean = this.t > maxT
    val pred_pop: Boolean = this.pop.size == 0
    val pred_deg: Boolean = this.eco.countNatural() == 0
    val pred_nat: Boolean = this.eco.countNatural() == this.eco.size
    (pred_time || pred_pop || pred_deg || (pred_nat && pred_pop))

  /**
  This function runs the world's dynamics:
  1- Get the natural connected components
  2- Get the ecosystem services flow
  3- Get resource production
  4- Calculate propensities
  5- Update the world
  @param maxT is the maximum simulation time
  @return the state of the Matrix at the end of the simulation
  */
  def simulate(maxT: Double): Matrix =

    val (ncc, es) = this.eco.ecosystemServiceFlow
    val res = this.eco.resourceProduction(es)
    val popp = this.pop.demographicPropensities(0.0,res)
    val spontp = this.eco.spontaneousPropensities(popp._2,es)
    val tcp = this.pop.totalConversionPropensity(res)

    @tailrec
    def rec(
      world: Matrix,
      maxT: Double,
      ncc: VertexRDD[VertexId],
      es: Graph[(EcoUnit,Double),Long],
      res: Double,
      popp: (Double,Double),
      spontp: ((ListMap[VertexId,Double],ListMap[VertexId,Double],ListMap[VertexId,Double],ListMap[VertexId,Double]),Double),
      tcp: Double):
      Matrix =
        // return the current state of the Matrix if the simulation is to stop
        if world.doesNotHaveNext(maxT) then world

        else {
          // get the new world and in function of the event type actualize
          // propensities and/or ecosystem services or not
          val new_world: (Matrix, EventType) = Matrix.update(popp,spontp,tcp,world)
          // match the event type
          new_world._2 match {
            case Demographic => { // only the population and conversion propensities are updated
              val new_ncc = ncc
              val new_es = es
              val new_res = res
              val new_popp = new_world._1.pop.demographicPropensities(0.0,new_res)
              val new_spontp = spontp
              val new_tcp = new_world._1.pop.totalConversionPropensity(new_res)
            }
            case HighIntensityFertilityLoss => { // ecosystem service provision is unchange d by this event, sustantial time gain
              val new_ncc = ncc
              val new_es = es
              val new_res = res - 1.0 // this is just loosing one high intensity unit
              val new_popp = new_world._1.pop.demographicPropensities(0.0,new_res)
              val new_spontp = new_world._1.eco.spontaneousPropensities(new_popp._2,new_es)
              val new_tcp = new_world._1.pop.totalConversionPropensity(new_res)
            }
            case other => { // for any other event update everything
              val (new_ncc, new_es) = new_world._1.eco.ecosystemServiceFlow
              val new_res = new_world._1.eco.resourceProduction(new_es)
              val new_popp = new_world._1.pop.demographicPropensities(0.0,new_res)
              val new_spontp = new_world._1.eco.spontaneousPropensities(popp._2,new_es)
              val new_tcp = new_world._1.pop.totalConversionPropensity(new_res)
            }
          }
          rec(new_world._1,maxT,new_ncc,new_es,new_res,new_popp,new_spontp,new_tcp)
        }

      rec(this,maxT,ncc,es,res,popp,spontp,tcp)


object Matrix :

  /**
  Each of the constituants of the Matrix are initialized individually in a
  scala task in OpenMole and then fed to this Matrix constructor after being
  dealt. By default time at the construction is set to  0.0.
  */
  def apply(
    eco: EcoLandscape,
    pln: PlnLandscape,
    mng: MngLandscape,
    pop: HumanPop):
    Matrix =
      Matrix(0.0,eco,pln,mng,pop)

  /**
  @param world is this Matrix
  @param new_t is the new time after the event
  @param new_eco is the new ecological landscape with updated composition
  @return a new world with the updated ecological landscape
  */
  def updateEco(
    world: Matrix,
    new_t: Double,
    new_eco: EcoLandscape):
    Matrix =
      world.copy(t = new_t, eco = new_eco)

  /**
  @param world is this Matrix
  @param new_t is the new time after the event
  @param new_eco is the new human population with updated size
  @return a world with an updated population
  */
  def updatePop(
    world: Matrix,
    new_t: Double,
    new_pop: Double):
    Matrix =
      world.copy(t = new_t, pop = new_pop)

  /**
  @param pop is the human population propensity
  @param spont are the spontaneous propensities
  @param tcp is the total conversion propensity
  @return a tuple with the updated world given the propensities and the event type
  */
  def update(
    pop: (Double,Double),
    spont: ((ListMap[VertexId,Double],ListMap[VertexId,Double],ListMap[VertexId,Double],ListMap[VertexId,Double]),Double),
    tcp: Double,
    world: Matrix):
    (Matrix, EventType) = {

    val new_t: Double = world.t - 1/log(rnd.nextDouble(pop._2 + spont._2 + tcp))
    // random number to select an event, maximum is the sum of the cumulative propensities
    val x_rnd: Double = rnd.nextDouble(pop._2 + spont._2 + tcp)

    selectEventType(x_rnd,pop._2,spont._2,tcp) match {
      case Demographic => {
        val upd_pop: HumanPop = applyDemographicEvent(x_rnd,pop,world.pop)
        (world.copy(t = new_t, pop = upd_pop), Demographic)
      }
      case Spontaneous => {
        selectSpontaneous(x_rnd,spont._1) match {
          case Recovery => {
            val upd_eco: Graph[EcoUnit,Long] = applySpontaneousEvent(x_rnd,spont._1._1,eco,Natural)
            val event = Recovery
          }
          case Degradation => {
            val upd_eco: Graph[EcoUnit,Long] = applySpontaneousEvent(x_rnd,spont._1._2,eco,Degraded)
            val event = Degradation
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
        (world.copy(t = new_t, eco = upd_eco),event)
      }
      case Conversion => {
        val upd_eco: Graph[EcoUnit,Long] = applyConversionEvent(x_rnd,spont._2,world.eco,world.pln,world.mng,tcp)
        (world.copy(t = new_t, eco = upd_eco),Conversion)
      }
    }
  }

  def applyDemographicEvent(
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
    eco: EcoLandscape,
    pln: PlanningLandscape,
    mng: ManagementLandscape,
    tcp: Double):
    EcoLandscape =

      // calculating propensities of management units and selecting the unit
      // where conversino will take place
      val mngp: ListMap[VertexId,Double] =
       mng.propensityOfMngUnits(ival,tcp,pln.composition,eco.composition)
      val mid: VertexId =
       mng.selectVId(x_rnd,mngp)

      val max: Double = mngp.get(vid)

      // this approach works because every management unit can be selected with
      // unifrom probability: every unit has the same total conversion propensity.
      // Else it is needed to access the previous element
      val utcp: Double = mngp.head._2
      val ival2: Double = max-utcp

      // calculating propensities of planning units within the management unit
      // and selecting the one to be converted
      val plnp: ListMap[VertexId,Double] =
       mng.lookup(mid).propensityOfPlnUnits(ival2,utcp,pln,eco.composition)
      val pid: VertexId =
       pln.selectVId(x_rnd,plnp)

      val vids: VertexRDD[VertexId] = pln.lookup(pid).composition
      mng.lookup(mid).stg match{
       case LandSharing => eco.update(vids, LowIntensity)
       case LandSparing => eco.update(vids, HighIntensity)
      }

  /**
  @param x_rnd is the random number thrown to sample the distributions
  @param spont is the total spontaneous propensity + the population one
  @param mng is the total management propensity + spontaneous + population
  @param pop is the total population propensity
  @return the event type
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
  @param prop contains the recovery, degradation and fertility loss propensities in field 1,2 and 3 respectively
  @return the event type
  */
  def selectSpontaneous(
    x_rnd: Double,
    prop: (ListMap[VertexId,Double],ListMap[VertexId,Double],ListMap[VertexId,Double],ListMap[VertexId,Double])):
    EventType =
      x_rnd match {
        case x if x < prop._1.last._2 => Recovery
        case x if x < prop._2.last._2 => Degradation
        case x if x < prop._3.last._2 => LowIntensityFertilityLoss
        case x if x < prop._4.last._2 => HighIntensityFertilityLoss
      }

  /**
  @param x_rnd is the random number thrown to sample the distributions
  @param prop contains the birth and death propensities in field 1 and 2 respectively
  @return the event type
  */
  def selectBirthOrDeath(
    x_rnd: Double,
    prop: (Double, Double)):
    EventType =
      x_rnd match {
        case x if x < prop._1 => Birth
        case x if x < prop._2 => Death
      }

end Matrix
