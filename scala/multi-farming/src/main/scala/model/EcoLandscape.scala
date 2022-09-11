package model
/*
import org.apache.spark.*
import org.apache.spark.graphx.*
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph
*/
import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.math.pow
import scala.util.Random as rnd
import scala.collection.parallel.immutable.ParVector

import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

/**
 *
 * Implementation of the landscape at a biophysical level. An EcoLandscape is composed by EcoUnits stored in a graph where
 * edges represent functional connectivity between units rather that adjacency in space. The class parameters are:
 * ecr: Ecological Connectivity Range
 * scal_exp: the scaling exponent of the power-law ecosystem services-area relationship
 * yes: contribution of ecosystem services to agricultural production in low-intensity units
 * his: the number of households that can be supported by a single high-intensity unit. This is used in the production
 * function.
 * s_rec: sensitivity of land recovery propensity to ecosystem service provision. Higher sensitivity means more response
 * to ecosystem services.
 * s_deg: idem for degradation.
 * s_flo: idem for fertility loss.
 *
 * The EcoLandscape is first instantiated to initialize the PlnLandscape, MngLandscape and HumanPop, and then initialized
 * using these three objects to produce an initial state as close to equilibrium with the population size as possible and
 * with a spatial structure that mimics the spatial processes of the simulation, given the parameters. This effort in
 * coherence, instead of just a random landscape, is to guarantee that the transient at the beginning of the simulation
 * is as short as possible and that the simulation does not start with a regime change.
 * @todo where should the spark context be declared and setted, if at some top level class then I might need to pass it
 *       as argument to a bunch of functions
*/
case class EcoLandscape(
  composition: Graph[EcoUnit,UnDiEdge],
  size: Int,
  ecr: Int,
  scal_exp: Double,
  yes: Double,
  his: Double,
  s_rec: Double,
  s_deg: Double,
  s_flo: Double)
  extends BaseLandscape[EcoUnit] with Agriculture with EcoServices with SpontaneousPropensities with SpatialStochasticEvents :

    /**
     * Updates land cover in multiple units following a conversion event.
     * @param vids is a collection of VertexIDs to be updated in cover
     * @param cover is the new cover
     * @return an updated EcoLandscape
     **/
    def update(old_units: ParVector[EcoUnit], new_units: ParVector[EcoUnit]): EcoLandscape =
      val comp = this.updateComposition(old_units,new_units)
      this.copy(composition = comp)

    /**
     * Updates land cover in a single unit following a spontaneous event.
     * @param vid  is the VertexID to be updated in cover
     * @param cover is the new cover
     * @return an updated EcoLandscape
     * */
    def update(old_unit: EcoUnit, new_unit: EcoUnit): EcoLandscape =
      val comp = this.updateComposition(old_unit,new_unit)
      this.copy(composition = comp)

    /**
     * Determines in which units a conversion event happens and what is the resulting cover from the transition:
     * LowIntensity or HighIntensity. The procedure starts by spatializing the conversion propensity over the management
     * landscape to choose a management unit and repeating this procedure over the planning landscape until reaching the
     * ecological level. Type of resulting land cover is determined by the strategy in the management unit where the
     * transition happens.
     * @param x_rnd the random number to sample the probability distribution.
     * @param ival initial value to calculate the propensity in each MngUnit.
     * @param pln the planning landscape.
     * @param mng the management landscape.
     * @param tcp the total conversion propensity.
     * @return a tuple with the unit's VertexIds and the resulting cover.
     * */
    def resolveConversionEvent(
      x_rnd: Double,
      ival: Double,
      pln: PlnLandscape,
      mng: MngLandscape,
      tcp: Double):
    (ParVector[EcoUnit], ParVector[EcoUnit]) =

      // calculating propensities of management units and selecting the unit
      // where conversion will take place
      val mngp: ListMap[MngUnit, Double] =
      mng.propensityOfMngUnits(ival, tcp, pln.composition, this.composition)
      val mid: VertexId =
        mng.selectVId(x_rnd, mngp)

      val max: Double = mngp.getOrElse(mid,0.0) // should warn that 0.0 means something went wrong

      // this approach works because every management unit can be selected with
      // uniform probability: every unit has the same total conversion propensity.
      // Else it is needed to access the previous element
      val utcp: Double = mngp.head._2
      val ival2: Double = max - utcp

      // calculating propensities of planning units within the management unit
      // and selecting the one to be converted
      val plnp: ListMap[VertexId, Double] =
      mng.composition.vertices.lookup(mid).head.propensityOfPlnUnits(ival2, utcp, pln, this.composition)
      val pid: VertexId =
        pln.selectVId(x_rnd, plnp)

      val vids: ParVector[VertexId] = pln.composition.vertices.lookup(pid).head.composition
      mng.composition.vertices.lookup(mid).head.strategy match {
        case MngStrategy.LandSharing => (vids, EcoUnit(LandCover.LowIntensity) )
        case MngStrategy.LandSparing => (vids, EcoUnit(LandCover.HighIntensity) )
      }

    def initialize(pln: PlnLandscape, mng: MngLandscape, fagr: Double, fdeg: Double): EcoLandscape =
      EcoLandscape.initialize(this,pln,mng,fagr,fdeg)

    def countNatural: Int = this.composition.vertices.filter( (_,eu) => eu.matchCover(LandCover.Natural) ).count.toInt
    def countDegraded: Int = this.composition.vertices.filter( (_,eu) => eu.matchCover(LandCover.Degraded) ).count.toInt
    def countAgriculturalLI: Int = this.composition.vertices.filter( (_,eu) => eu.matchCover(LandCover.LowIntensity) ).count.toInt
    def countAgriculturalHI: Int = this.composition.vertices.filter( (_,eu) => eu.matchCover(LandCover.HighIntensity) ).count.toInt
    def countAgricultural: Int = this.countAgriculturalLI + this.countAgriculturalHI

object EcoLandscape :

  /**
  *This method overloads the pre-given apply method and is to be used when initializing the system. The function builds
  *a fully natural EcoLandscape from the EcoLandscape parameter values. Initialization with simulation's initial values
  *is done in initialize function defined in the companion case class.
  * @constructor
  * @param r is the landscape's radius
  * @param ecr is the ecological connectivity range and determines biophysical connections between units
  * @param scal_exp is the scaling exponent of the power-law ecosystem services - area relationship
  * @param yes is the contribution of ecosystem services to production in low-intensity units
  * @param his is the number of households that are sustained by one high-intensity unit
  * @param s_rec is land recovery sensitivity to ecosystem service provision
  * @param s_deg is land degradation sensitivity to ecosystem service provision
  * @param s_flo is fertility loss sensitivity to ecosystem service provision
  * @return an EcoLandscape
  */

  def apply(
    r: Int,
    ecr: Int,
    scal_exp: Double,
    yes: Double,
    his: Double,
    s_rec: Double,
    s_deg: Double,
    s_flo: Double):
  EcoLandscape =
    val comp = buildComposition(r,ecr)
    EcoLandscape(comp,ModCo.area(r),ecr,scal_exp,yes,his,s_rec,s_deg,s_flo)


  /**
  @param r is the radius of the biophysical landscape
  @param ecr is the ecological connectivity range
  @return the biophysical composition graph with every unit in a natural state
  */
  def buildComposition(
    r: Int,
    ecr: Int): 
  Graph[EcoUnit, UnDiEdge] =
    // first create a list of nodes to feed to the graph constructor
    val nodes: List[EcoUnit] =
      ModCo.apply(r).map { m => EcoUnit(m.toLong,LandCover.Natural) }.toList
    // now use the list of nodes to create a list of existing edges based on neighborhood
    val edges: List[UnDiEdge] =
      nodes.toSet.subsets(2).collect{
          case s if ModCo.neighbors(s.head.id,r,ecr).contains(s.last.id) =>
            UnDiEdge( s.head, s.last )
        }.toList
    // instantiate the graph
    Graph.from(nodes,edges)

  /**
  @param fagr is fraction of agricultural units in the initial biophysical landscape
  @param fdeg is fraction of degraded units in the initial biophysical landscape
  @return a biophysical landscape initialized according to the simulation parameter values
  */
  def initialize(
    eco: EcoLandscape,
    pln: PlnLandscape,
    mng: MngLandscape,
    fagr: Double,
    fdeg: Double):
    EcoLandscape =
      /**
      TODO: check this
      */
      def initializeAgriculturalUnit(
        eco: EcoLandscape,
        pln: PlnLandscape,
        mng: MngLandscape):
      EcoLandscape =
        val x_rnd: Double = rnd.nextDouble( )
        val res = eco.resolveConversionEvent(x_rnd,0.0,pln,mng,1.0)
        eco.update(res._1,res._2)

      // TODO: update function takes vid independently of EcoUnit, now the vid is in the EcoUnit
      def initializeDegradedUnit(eco: EcoLandscape): EcoLandscape =
        val propensity = eco.degradationPropensity(0.0, eco.ecoServices , 1.0)
        val x_rnd = rnd.between(0.0,propensity.last._2)
        val vid = eco.selectVId(x_rnd,propensity)
        eco.update(vid, EcoUnit(LandCover.Degraded))
    
      /**
      @param n is a tuple with the number of remaining agricultural units to put first and the remaining degraded units second
      @param transition is the type of transition that was simulated
      @param step is the number of units that have transitioned
      @return a new tuple with the updated numbers
      */
      def updateRemaining(
        n: (Int,Int),
        transition: EventType,
        step: Int):
      (Int,Int) =
        transition match{
          case EventType.Conversion =>
            val upd_n_deg = n._2
            if n._1>0 then {
              val upd_n_agr = n._1 - step
              (upd_n_agr,upd_n_deg)
            }
            else {
              val upd_n_agr = n._1
              (upd_n_agr,upd_n_deg)
            }
          case EventType.Degradation =>
            val upd_n_agr = n._1
            if n._2>0 then {
              val upd_n_deg = n._2 - step
              (upd_n_agr,upd_n_deg)
            }
            else {
              val upd_n_deg = n._1
              (upd_n_agr, upd_n_deg)
            }
        }

      @tailrec
      def rec(
        eco: EcoLandscape,
        pln: PlnLandscape,
        mng: MngLandscape,
        n_agr: Int,
        n_deg: Int): 
        EcoLandscape =
          val n: Int = n_agr + n_deg
          if n==0 then eco 
          else {
            rnd.nextInt(n) match {
              case n_rnd if n_rnd<n_agr =>  // Conversion transition is chosen
                val old_agr: Int = eco.countAgricultural
                val upd_eco: EcoLandscape = initializeAgriculturalUnit(eco, pln, mng)
                val new_agr: Int = upd_eco.countAgricultural
                val step: Int = new_agr - old_agr
                val n_remaining: (Int,Int) = updateRemaining((n_agr,n_deg),EventType.Conversion,step)
                rec(upd_eco, pln, mng, n_remaining._1, n_remaining._2)
              
              case n_rnd if n_rnd<n_deg =>  // Degradation transition is chosen
                val upd_eco = initializeDegradedUnit(eco)
                val n_remaining: (Int,Int) = updateRemaining((n_agr,n_deg),EventType.Degradation,1)
                rec(upd_eco, pln, mng, n_remaining._1, n_remaining._2)
            }
          }

      val n_agr: Int = eco.size * fagr.toInt
      val n_deg: Int = eco.size * fdeg.toInt
      rec(eco, pln, mng, n_agr, n_deg)
          
end EcoLandscape
