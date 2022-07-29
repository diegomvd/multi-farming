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
TODO: I am carrying a bunch of crap on memory just to avoid calculating ncc and the
rest in the case that it is a high intensity fertility loss transition. This
might not be worth it, since it complexifies code-wise and probably loads too much
stuff on memory.
*/
case class EcoLandscape(
  composition: Graph[EcoUnit,Long],
  naturalcc: VertexRDD[VertexId],
  ecoservices: VertexRDD[Double],
  production: Double,
  recprop: ListMap[VertexId, Map],
  degprop: ListMap[VertexId, Map],
  lflprop: ListMap[VertexId, Map],
  hflprop: ListMap[VertexId, Map],
  tsp: Double,
  size: Int,
  ecr: Int,
  scalexp: Double,
  yes: Double,
  his: Double,
  srec: Double,
  sdeg: Double,
  sflo: Double)
  extends Landscape with VoronoiTesselation with Agriculture with EcoServices with SpontaneousPropensities:

  def update(vids: VertexRDD[VertexId], transition: TransitionType, cover: EcoUnit): EcoLandscape =
    EcoLandscape.update(this,vids,transition,cover)

  def update(vid: VertexId, transition: TransitionType, cover: EcoUnit): EcoLandscape =
    EcoLandscape.update(this,vids,transition,cover)

  def initialize(pln: PlnLandscape, mng: MngLandscape, fagr: Double, deg: Double): EcoLandscape =
    EcoLandscape.initialize(this,pln,mng,fagr,fdeg)

  def countNatural: Int = this.composition.vertices.filter( (_,eu) => eu.isNatural ).count.toInt
  def countDegraded: Int = this.composition.vertices.filter( (_,eu) => eu.isDegraded ).count.toInt
  def countAgricultural: Int = this.composition.vertices.filter( (_,eu) => eu.isDegraded ).count.toInt


object EcoLandscape :

  def update(
    eco: EcoLandscape,
    vids: VertexRDD[VertexId],
    transition: TransitionType,
    cover: EcoUnit): EcoLandscape =
      val comp = eco.updateComposition(eco.composition,vids,cover)
      updateMetrics(comp,eco,transition)

  def update(
    eco: EcoLandscape,
    vid: VertexId,
    transition: TransitionType,
    cover: EcoUnit): EcoLandscape =
      val comp = eco.updateComposition(eco.composition,vid,cover)
      updateMetrics(comp,eco,transition)

  def updateMetrics(
    comp: Graph[EcoUnit,Long],
    eco: EcoLandscape,
    transition: TransitionType): EcoLandscape =
      transition match {
        case HighIntensityFertLoss => {
          val prod = eco.updateProduction(eco.production)
          val joint = joinCompAndEcoServices(comp,eco.ecoservices)
          val ((recp, degp, lflp, hflp), upd_tsp) = eco.updatePropensities(0.0,joint,(srec,sdeg,sflo))
          eco.copy(composition = comp, production = prod, recprop = recp, degprop = degp, lflprop = lflp, hflprop = hflp, tsp = upd_tsp)
        }
        case other => {
          val (ncc,es) = this.updateEcoServices(comp,eco.scalexp,eco.size)
          val joint = EcoLandscape.joinCompAndEcoServices(comp,es)
          val prod = this.updateProduction(joint,yes,his)
          val ((recp, degp, lflp, hflp), upd_tsp) = eco.updatePropensities(0.0,joint,(srec,sdeg,sflo))
          eco.copy(composition = comp, naturalcc = ncc, ecoservices = es, production = prod, recprop = recp, degprop = degp, lflprop = lflp, hflprop = hflp, tsp = upd_tsp)
        }
      }


  /**
  This method overloads the pre-given apply method and is to be used when initia
  lizing the system. The function build a fully natural EcoLandscape from the
  EcoLandscape parameter values. Initialization with simulation's initial values
  is done in initialize function
  */
  def apply(
    size: Int,
    ecr: Int,
    scalexp: Double,
    yes: Double,
    his: Double,
    srec: Double,
    sdeg: Double,
    sflo: Double
  ): EcoLandscape = {
    val comp = buildComposition(r,ecr)
    val (ncc,es) = EcoServices.calculateEcoServices(ecocomp,scalexp,size)
    val prod = Agriculture.calculateProduction(joinCompAndEcoServices(comp,es),yes,his)
    val ((recp, degp, lflp, hflp), tsp) = SpontaneousPropensities.calculatePropensities(0.0,joint,(srec,sdeg,sflo))
    EcoLandscape(comp,ncc,es,prod,recp,degp,lflp,hflp,tsp,size,ecr,scalexp,yes,his,srec,sdeg,sflo)
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
        val propensity = eco.degradationPropensity(0.0, joinCompAndEcoServices(comp,es), 1.0)
        val xrnd = rnd.nextDouble(propensity.last._2)
        val vid = eco.selectVId(xrnd,propensity)
        eco.update(vid, Degradation, Degraded)
      }

      /**
      @param n is a tuple with the number of remaining agricultural units to put first and the remaining degraded units second
      @param transition is the type of transition that was simulated
      @param step is the number of units that have transitioned
      @return a new tuple with the updated numbers
      */
      def updateRemaining(
        n: (Int,Int),
        transition: String,
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
              n_deg: Int): Graph[EcoUnit,Long] = {

        val n: Int = n_agr + n_deg
        if (n==0){
          eco
        }
        else {
          rnd.nextInt(n) match {
            case n_rnd if n_rnd<n_agr => { // Conversion transition is chosen
              val old_agr: Int = eco.countAgricultural
              val upd_eco: Graph[EcoUnit,Long] = initAgriculturalUnit(eco,pln,mng)
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
