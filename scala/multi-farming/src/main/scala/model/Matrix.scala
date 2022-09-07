package model
import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import org.apache.spark.*
import org.apache.spark.graphx.*
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph
import scala.math.log
import scala.util.Random as rnd

/**
 * Implementation of the complete social-ecological system. The Matrix is the digital representation of the system and
 * is defined by its internal time and the sub-components of the system: the ecological, planning and management
 * landscapes and the human population depending on and managing the landscape.
 * @author diego
*/
case class Matrix(
  t: Double,
  eco: EcoLandscape,
  pln: PlnLandscape,
  mng: MngLandscape,
  pop: HumanPop):

  /**
   * Determines whether a simulation is to be terminated. A simulation is terminated if the Matrix's internal time
   * exceeds the maximum simulation time or if the system reaches ana absorbant state: null population size or fully
   * degraded landscape (even if population is not null it will eventually die-off).
   * @param maxT the maximum simulation time.
   * @return True if the simulation is to be terminated, False if not.
  */
  def doesNotHaveNext(maxT: Double): Boolean =
    val pred_time: Boolean = this.t > maxT
    val pred_pop: Boolean = this.pop.size == 0
    val pred_deg: Boolean = this.eco.countNatural == 0
    pred_time || pred_pop || pred_deg

  /**
   * Runs a simulation of the Matrix's dynamics. Each simulation step consists on:
   * 1- Calculate the ecosystem services flow in the ecological landscape,
   * 2- Calculate resource production,
   * 3- Calculate the propensities of each event,
   * 4- Randomly choose a next event time and next event type,
   * 5- Update the Matrix's state: time and change in landscape or population.
   * @param maxT the maximum simulation time.
   * @return the state of the Matrix at the end of the simulation.
  */
  def simulate(maxT: Double): Matrix =

    /**
     * Tail recursive function describing each simulation step.
     * @param world the Matrix being simulated.
     * @param maxT the maximum simulation time.
     * @param ncc the ecological landscape's natural connected components.
     * @param es the joined ecosystem services flow graph.
     * @param res the amount of resources available for the population.
     * @param popp the propensities of birth and death.
     * @param spontp the propensities of spontaneous landscape events.
     * @param tcp the total conversion propensity.
     * @return the state of the Matrix at the end of the simulation step.
     * */
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
        val (new_world, event): (Matrix, EventType) = Matrix.update(popp,spontp,tcp,world)
        // match the event type
        event match {
          case EventType.Demographic =>  // only the population and conversion propensities are updated
            val new_popp = new_world.pop.demographicPropensities(0.0,res)
            val new_tcp = new_world.pop.totalConversionPropensity(res)
            rec(new_world,maxT,ncc,es,res,new_popp,spontp,new_tcp)

          case EventType.HighIntensityFertilityLoss => // ecosystem service provision is unchanged by this event, substantial time gain
            val new_res = res - 1.0 // this is just loosing one high intensity unit
            val new_popp = new_world.pop.demographicPropensities(0.0,new_res)
            val new_spontp = new_world.eco.spontaneousPropensities(new_popp._2,es)
            val new_tcp = new_world.pop.totalConversionPropensity(new_res)
            rec(new_world,maxT,ncc,es,new_res,new_popp,new_spontp,new_tcp)

          case _ =>  // for any other event update everything
            val (new_ncc, new_es): (VertexRDD[VertexId], Graph[(EcoUnit,Double),Long]) = new_world.eco.ecosystemServiceFlow
            val new_res = new_world.eco.resourceProduction(new_es)
            val new_popp = new_world.pop.demographicPropensities(0.0,new_res)
            val new_spontp = new_world.eco.spontaneousPropensities(popp._2,new_es)
            val new_tcp = new_world.pop.totalConversionPropensity(new_res)
            rec(new_world,maxT,new_ncc,new_es,new_res,new_popp,new_spontp,new_tcp)
        }
      }

    val (ncc, es): (VertexRDD[VertexId], Graph[(EcoUnit,Double),Long]) = this.eco.ecosystemServiceFlow
    val res = this.eco.resourceProduction(es)
    val popp = this.pop.demographicPropensities(0.0, res)
    val spontp = this.eco.spontaneousPropensities(popp._2, es)
    val tcp = this.pop.totalConversionPropensity(res)

    rec(this, maxT, ncc, es, res, popp, spontp, tcp)

object Matrix :

  /**
   * Matrix constructor at time 0.0.
   * @param eco the ecological landscape.
   * @param pln the planning landscape.
   * @param mng the management landscape.
   * @param pop the human population.
   * @return an initialized Matrix ready to be simulated.
   * @note the subcomponents of the matrix are initialized individually inside a ScalaTask within OpenMole and then
   *       this constructor is called in the same ScalaTask.
  */
  def apply(
    eco: EcoLandscape,
    pln: PlnLandscape,
    mng: MngLandscape,
    pop: HumanPop):
  Matrix =
    Matrix(0.0,eco,pln,mng,pop)

  /**
   * Updates the Matrix's state by choosing next event time and type from the event propensities.
   * @param pop    is the human population propensity
   * @param spont  are the spontaneous propensities
   * @param tcp    is the total conversion propensity
   * @return a tuple with the updated world and the event type
  */
  def update(
    pop: (Double,Double),
    spont: ((ListMap[VertexId,Double],ListMap[VertexId,Double],ListMap[VertexId,Double],ListMap[VertexId,Double]),Double),
    tcp: Double,
    world: Matrix):
  (Matrix, EventType) =

    // expression for the next event time
    val new_t: Double = world.t - 1.0/log(rnd.between(0.0, pop._2 + spont._2 + tcp))
    // random number to select an event, maximum is the sum of the cumulative propensities
    val x_rnd: Double = rnd.between(0.0, pop._2 + spont._2 + tcp)

    selectEventType(x_rnd,pop._2,spont._2,tcp) match {
      case EventType.Demographic =>
        val upd_pop: HumanPop = world.pop.update(HumanPop.selectBirthOrDeath(x_rnd,pop))
        (world.copy(t = new_t, pop = upd_pop), EventType.Demographic)
      case EventType.Spontaneous =>
        world.eco.selectSpontaneous(x_rnd,spont._1) match {
          case EventType.Recovery =>
            val upd_eco: EcoLandscape = world.eco.update(world.eco.selectVId(x_rnd, spont._1._1), EcoUnit(LandCover.Natural))
            (world.copy(t = new_t, eco = upd_eco), EventType.Recovery)
          case EventType.Degradation =>
            val upd_eco: EcoLandscape = world.eco.update( world.eco.selectVId(x_rnd, spont._1._2), EcoUnit(LandCover.Degraded))
            (world.copy(t = new_t, eco = upd_eco), EventType.Degradation)
          case EventType.LowIntensityFertilityLoss =>
            val upd_eco: EcoLandscape = world.eco.update( world.eco.selectVId(x_rnd, spont._1._3), EcoUnit(LandCover.Natural))
            (world.copy(t = new_t, eco = upd_eco), EventType.LowIntensityFertilityLoss)
          case EventType.HighIntensityFertilityLoss =>
            val upd_eco: EcoLandscape = world.eco.update( world.eco.selectVId(x_rnd, spont._1._4), EcoUnit(LandCover.Degraded))
            (world.copy(t = new_t, eco = upd_eco), EventType.HighIntensityFertilityLoss)
        }
      case EventType.Conversion =>
        val (vids, cover): (VertexRDD[VertexId], EcoUnit) = world.eco.resolveConversionEvent(x_rnd,spont._2,world.pln,world.mng,tcp)
        val upd_eco: EcoLandscape = world.eco.update(vids, cover)
        (world.copy(t = new_t, eco = upd_eco), EventType.Conversion)
    }

  /**
   * Determines the type of the next event's given the propensities.
   * @param x_rnd is the random number thrown to sample the distributions.
   * @param pop  is the total population propensity.
   * @param spont is the total spontaneous + population propensities.
   * @param tcp  is the total management + spontaneous + population propensities.
   * @return the next event's type.
  */
  def selectEventType(
    x_rnd: Double,
    pop: Double,
    spont: Double,
    tcp: Double):
  EventType =
    x_rnd match {
      case x if x < pop => EventType.Demographic
      case x if x < spont => EventType.Spontaneous
      case x if x < tcp => EventType.Conversion
   }

end Matrix
