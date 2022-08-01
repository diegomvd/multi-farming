/**
This object provides all the funcitons to manipulate a biophysical landscape
represented as a graph of ecological units connected by their manhattan distance
in an hexagonal lattice. An ecological landscape is built given its radius and
the ecological connectivity range. The functions defined in the object provide
the following functionalities:

1- Build, initialize and update a composition graph
2- Calculate connected natural components and ecosystem service flow
3- Calculate cummulative propensities of spontaneous land-cover transitions
4- Calculate resource production at the whole landscape level
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
    EcoLandscape = {

      val n_agr: Int = size*fagr.toInt
      val n_deg: Int = size*fdeg.toInt

      /**
      TODO: check this
      */
      def initializeAgriculturalUnit(
        eco: EcoLandscape,
        pln: PlnLandscape,
        mng: MngLandscape):
        Graph[EcoUnit,Long] = {
          val x_rnd: Double = rnd.nextDouble( 1.0 )
          World.applyConversionEvent(x_rnd,eco,pln,mng,1.0)
      }

      def initializeDegradedUnit(eco: EcoLandscape): EcoLandscape = {
        val propensity = eco.degradationPropensity(0.0, joinCompAndEcoServices(eco.composition,eco.ecosystemServicesFlow), 1.0)
        val xrnd = rnd.nextDouble(propensity.last._2)
        val vid = eco.selectVId(xrnd,propensity)
        eco.update(vid, Degraded)
      }

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
        (Int,Int) = {
          transition match{
            case Conversion =>{
              val upd_n_deg = n._2
              if n._1>0 { val upd_n_agr = n._1 - step }
              else {val upd_n_agr = n._1}
            }
            case Degradation =>{
              val upd_n_agr = n._1
              if n._2>0 { val upd_n_deg = n._2 - step }
              else {val upd_n_deg = n._1}
            }
          }
          (upd_n_agr,upd_n_deg)
        }

      @annotation.tailrec
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
              val step: new_agr - old_agr
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
    }

  /**
  @return a graph joining the ecounits with the ES flow they receive
  */
  def joinCompAndEcoServices(
    comp: Graph[EcoUnit,Long],
    es: VertexRDD[Double]):
    Graph[(EcoUnit,Double),Long] = {
     comp.outerJoinVertices(es){ (vid, eu, es_opt) =>
       es_opt match {
         case Some(es) => (eu, es)
         case None => (eu, 0.0)
       }
     }
  }
end EcoLandscape
