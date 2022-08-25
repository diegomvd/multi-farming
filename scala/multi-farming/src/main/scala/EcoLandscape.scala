/**
The Ecological Landscape is implemented in the EcoLandscape case class and
companion object. An ecological landscape is a group of Ecological Units. The
composition is stored in a graph where edges describe functional connectivity
and not adjacency in space. The distance for functional connectivity is determined
by the Ecological Connectivity Range parameter (ecr).There is a number of parameters
of the EcoLandscape related to :

  @param ecr: Ecological Connecvitiy Range
  @param scalexp: the scaling exponent of the power-law ecosystem services-area
  relationship
  @param yes: contribution of ecosystem services to agricultural production in
  low-intensity units
  @param his: the number of households that can be supported by a single high-
  intensity unit. This is used in the production function.
  @param srec: sensitivity of land recovery propensity to ecosystem service
  provision. Higher sensitivity means more response to ecosystem services.
  @param sdeg: idem for degradation.
  @param sflo: idem for fertility loss.

The EcoLandscape must be first built with the apply method of the companion object
and then initialized since it requires the generation of planning and management
landscape that depend on the structure for that. The initialized EcoLandscape is
as close to equilibrium with the population size as possible and with a spatial
structure built mimicking the spatial processes f the simulation given the parameters.
This effort in coherence, instead of just a random landscape, is to guarantee that
the transient at the beginning of the simulation is as short as possible and that
we do not start with a regime change.

Key functions are:
  - Update the landscape composition.
  - Initialize the landscape.
  - Count types of units as a metric.
*/

import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph
import scala.math.pow

/**
The ecological landscape class. The landscape is only given by comp, which is
a Graph of EcoUnits, the rest of parameters are useful to retrieve landscape
metrics without need of calculation and make them persist over simulation time.

*/
case class EcoLandscape(
  composition: Graph[EcoUnit,Long],
  size: Int,
  ecr: Int,
  scalexp: Double,
  yes: Double,
  his: Double,
  srec: Double,
  sdeg: Double,
  sflo: Double)
  extends BaseLandscape with Agriculture with EcoServices with SpontaneousPropensities with SpatialStochasticEvents :

    def update(vids: VertexRDD[VertexId], cover: EcoUnit): EcoLandscape =
      val comp = this.updateComposition(vids,cover)
      this.copy(composition = comp)

    def update(vid: VertexId, cover: EcoUnit): EcoLandscape =
      val comp = this.updateComposition(vid,cover)
      this.copy(composition = comp)

    def initialize(pln: PlnLandscape, mng: MngLandscape, fagr: Double, fdeg: Double): EcoLandscape =
      EcoLandscape.initialize(this,pln,mng,fagr,fdeg)

    def countNatural: Int = this.composition.vertices.filter( (_,eu) => eu.matchCover(Natural) ).count.toInt
    def countDegraded: Int = this.composition.vertices.filter( (_,eu) => eu.matchCover(Degraded) ).count.toInt
    def countAgriculturalLI: Int = this.composition.vertices.filter( (_,eu) => eu.matchCover(LowIntensity) ).count.toInt
    def countAgriculturalHI: Int = this.composition.vertices.filter( (_,eu) => eu.matchCover(HighIntensity) ).count.toInt
    def countAgricultural: Int = this.countAgriculturalLI + this.countAgriculturalHI

object EcoLandscape :

  /**
  This method overloads the pre-given apply method and is to be used when initia
  lizing the system. The function build a fully natural EcoLandscape from the
  EcoLandscape parameter values. Initialization with simulation's initial values
  is done in initialize function
  */
  def apply(
    r: Int,
    ecr: Int,
    scalexp: Double,
    yes: Double,
    his: Double,
    srec: Double,
    sdeg: Double,
    sflo: Double): EcoLandscape = {
      val comp = buildComposition(r,ecr)
      EcoLandscape(comp,ModCo.area(r),ecr,scalexp,yes,his,srec,sdeg,sflo)
  }

  /**
  @param r is the radius of the biophysical landscape
  @param ecr is the ecological connectivity range
  @return the biophysical composition graph with every unit in a natural state
  */
  def buildComposition(
    r: Int,
    ecr: Int): Graph[EcoUnit, Long] = {
    val sc: SparkContext
    val units: RDD[(VertexId, String)] =
      sc.parallelize( ModCo.apply(r).map{ (_.toLong,EcoUnit(Natural)) }.toSeq )
    val edges: RDD[Edge[Long]] =
      sc.parallelize( ModCo.apply(r).toSet.subsets(2).collect{
        case (pos1,pos2) if ModCo.neighbors(pos1,r,ecr).exists(_ == pos2) =>
          Edge(pos1.toLong,pos2.toLong,0L)
        }
      )
    Graph(units,edges)
  }

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

      val n_agr: Int = size*fagr.toInt
      val n_deg: Int = size*fdeg.toInt

      /**
      TODO: check this
      */
      def initializeAgriculturalUnit(
        eco: EcoLandscape,
        pln: PlnLandscape,
        mng: MngLandscape):
        Graph[EcoUnit,Long] =
          val x_rnd: Double = rnd.nextDouble( 1.0 )
          World.applyConversionEvent(x_rnd,eco,pln,mng,1.0)


      def initializeDegradedUnit(eco: EcoLandscape): EcoLandscape =
        val propensity = eco.degradationPropensity(0.0, joinCompAndEcoServices(eco.composition,eco.ecosystemServicesFlow), 1.0)
        val xrnd = rnd.nextDouble(propensity.last._2)
        val vid = eco.selectVId(xrnd,propensity)
        eco.update(vid, Degraded)


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
            case Conversion => {
              val upd_n_deg = n._2
              if n._1>0 then { val upd_n_agr = n._1 - step }
              else {val upd_n_agr = n._1}
            }
            case Degradation => {
              val upd_n_agr = n._1
              if n._2>0 then { val upd_n_deg = n._2 - step }
              else {val upd_n_deg = n._1}
            }
          }
          (upd_n_agr,upd_n_deg)

      @tailrec
      def rec(eco: EcoLandscape,
              pln: PlnLandscape,
              mng: MngLandscape,
              n_agr: Int,
              n_deg: Int): EcoLandscape = {

        val n: Int = n_agr + n_deg
        if (n==0){
          eco
        }
        else {
          rnd.nextInt(n) match {
            case n_rnd if n_rnd<n_agr => { // Conversion transition is chosen
              val old_agr: Int = eco.countAgricultural
              val upd_eco: Graph[EcoUnit,Long] = initializeAgriculturalUnit(eco,pln,mng)
              val new_agr: Int = upd_eco.countAgricultural
              val step: Int = new_agr - old_agr
              val n_remaining: (Int,Int) = updateRemaining((n_agr,n_deg),Conversion,step)
            }
            case n_rnd if n_rnd<n_deg => { // Degradation transition is chosen
              val upd_eco = initializeDegradedUnit(eco)
              val n_remaining: (Int,Int) = updateRemaining((n_agr,n_deg),Degradation,1)
            }
          }
          rec(upd_eco, plan, mng, n_remaining._1, n_remaining._2)
        }
      }
      rec(eco, pln, mng, n_agr, n_deg)


end EcoLandscape
