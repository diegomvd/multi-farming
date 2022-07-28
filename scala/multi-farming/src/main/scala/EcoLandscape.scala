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
class EcoLandscape(
  composition: Graph[EcoUnit,Long],
  naturalcc: VertexRDD[VertexId],
  ecoservices: VertexRDD[Double],
  production: Double,
  scalexp: Double,
  size: Int,
  yes: Double,
  his: Double
) extends Landscape with VoronoiTesselation with Agriculture with EcoServices :

  def update(vids: VertexRDD[VertexId], transition: TransitionType, cover: EcoUnit): EcoLandscape =
    val comp = this.updateComposition(this.composition,vids,cover)
    val (ncc,es) = this.updateEcoServices(comp,this.scalexp,this.size)
    val prod = this.updateProduction(EcoLandscape.joinCompAndEcoServices(comp,es),yes,his)
    new EcoLandscape(comp,ncc,es,prod,this.scalexp,this.size,this.yes,this.his)

  def update(vid: VertexId, transition: TransitionType, cover: EcoUnit): EcoLandscape =
    transition match {
      case HighIntensityFertLoss => {
        val comp = this.updateComposition(this.composition,vid,cover)
        val prod = this.updateProduction(this.production)
        new EcoLandscape(comp,this.naturalcc,this.ecoservices,prod,this.scalexp,this.size,this.yes,this.his)
      }
      case other => {
        val comp = this.updateComposition(this.composition,vid,transition)
        val (ncc,es) = this.updateEcoServices(comp,this.scalexp,this.size)
        val prod = this.updateProduction(EcoLandscape.joinCompAndEcoServices(comp,es),yes,his)
        new EcoLandscape(comp,ncc,es,prod,this.scalexp,this.size,this.yes,this.his)
      }
    }


  def init(pln: PlnLandscape, mng: MngLandscape, z: Double, fagr: Double, fdeg: Double): EcoLandscape =
    this.copy(EcoLandscape.init(this, pln, mng, z, fagr, fdeg))


  def propensities:



object EcoLandscape{

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
    z: Double,
    size: Int,
    fagr: Double,
    fdeg: Double): Graph[EcoUnit,Long] = {

    val n_agr: Int = size*fagr.toInt
    val n_deg: Int = size*fdeg.toInt

    @annotation.tailrec
    def rec(eco: Graph[EcoUnit,Long],
            pln: Graph[PlnUnit,Long],
            mng: Graph[MngUnit,Long],
            z: Double,
            size: Int,
            n_agr: Int,
            n_deg: Int): Graph[EcoUnit,Long] = {

      val n: Int = n_agr + n_deg
      if (n==0){
        eco
      }
      else {
        rnd.nextInt(n) match {
          case n_rnd if n_rnd<n_agr => {
            val old_agr: Int = eco.vertices.filter( (_,eu) => eu.isAgricultural() ).count()
            val upd_eco: Graph[EcoUnit,Long] = initAgriculturalUnit(eco,pln,mng)
            val new_agr: Int = upd_eco.vertices.filter( (_,eu) => eu.isAgricultural() ).count()
            val step: new_agr - old_agr
            val n_remaining: (Int,Int) = initUpdateRemaining((n_agr,n_deg),"Conversion",step)
          }
          case n_rnd if n_rnd<n_deg => {
            val upd_eco: Graph[EcoUnit,Long] = initDegradedUnit(eco,z,size)
            val n_remaining: (Int,Int) = initUpdateRemaining((n_agr,n_deg),"Degradation",1)
          }
        }
        rec(upd_eco, plan, mng, z, size, n_remaining._1, n_remaining._2)
      }
    }
    rec(eco.composition, pln.composition, mng.composition, z, size, n_agr, n_deg)
  }

  /**
  @param eco is the biophysical landscape graph
  @param z is the ecosystem services - area scaling
  @param size is the total number of units in the biophysical landscape
  @return the degradation propensity for the initialization
  */
  def initDegradationPropensity(eco: Graph[EcoUnit,Long],
                                z: Double,
                                size: Int): VertexRDD[Double] = {
    val es_graph: Graph[(EcoUnit,Double),Long] = EcosystemServices.esGraph(eco,es_flow)
    propensities(0.0,es_graph,1.0,Natural,EcoUnit.decreasingPES)
  }

  /**
  @param pln is the planning landscape graph
  @param mng is the management landscape graph
  @return an updated biophysical landscape graph after initializing agricultural units
  */
  def initAgriculturalUnit(eco: Graph[EcoUnit,Long],
                           pln: Graph[PlnUnit,Long],
                           mng: Graph[MngUnit,Long]): Graph[EcoUnit,Long] = {
    val x_rnd: Double = rnd.nextDouble( 1.0 )
    World.applyConversionEvent(x_rnd,eco,pln,mng,1.0)
  }

  /**
  @param z is the scaling exponent of the ecosystem services area relationship
  @param size is the number of ecological units in the biophysical landscape
  @return an updated biophysical landscape graph after initializing a degraded unit
  */
  def initDegradedUnit(eco: Graph[EcoUnit,Long],
                       z: Double,
                       size: Int){
    val prop: VertexRDD[Double] = initDegradationPropensity(eco,z,size)
    val x_rnd: Double = rnd.nextDouble( prop.reduce(_+_) )
    World.applySpontaneousEvent(x_rnd,prop,eco,"Degraded")
  }

  /**
  @param n is a tuple with the number of remaining agricultural units to put first and the remaining degraded units second
  @param event is the type of transition that was simulated
  @param step is the number of units that have transitioned
  @return a new tuple with the updated numbers
  */
  def initUpdateRemaining(n: (Int,Int),
                          event: String,
                          step: Int): (Int,Int) = {
    event match{
      case "Conversion" =>{
        val upd_n_deg = n._2
        if n._1>0 { val upd_n_agr = n._1 - step }
        else {val upd_n_agr = n._1}
      }
      case "Degradation" =>{
        val upd_n_agr = n._1
        if n._2>0 { val upd_n_deg = n._2 - step }
        else {val upd_n_deg = n._1}
      }
    }
    (upd_n_agr,upd_n_deg)
  }

  /**
  @param eco is the biophysical composition of the landscape
  @param es is the ecosystem service flow in each EcoUnit
  @return a graph joining the ecounits with the ES flow they receive
  */
  def esGraph(eco: Graph[EcoUnit,Long],
              es: VertexRDD[Double]): Graph[(EcoUnit,Double),Long] = {
     eco.outerJoinVertices(es){ (vid, eu, es_opt) =>
       es_opt match {
         case Some(es) => (eu, es)
         case None => (eu, 0.0)
       }
     }
  }

  /**
  This function returns the ecosystem services graph departing from ncc and
  performing the rest of intermediate steps inside
  */
  def esGraph(eco: Graph[EcoUnit,Long],
              ncc: VertexRDD[VertexId],
              z: Double,
              size: Int): Graph[(EcoUnit,Double),Long] = {
    val ncc_area: Map[(VertexId,VertexId),Long] = nccAreaDistribution(ncc)
    val area_graph: Graph[(EcoUnit,Double),Long] = nccAreaGraph(eco,ncc,ncc_area,size)
    val es_flow: VertexRDD[Double] = esFlow(area_graph,z)
    esGraph(eco,es_flow)
  }

  /**
  This function directly returns the ecosystem services graph performing the
  intermediate steps inside
  */
  def esGraph(eco: Graph[EcoUnit,Long],
              z: Double,
              size: Int): Graph[(EcoUnit,Double),Long] = {
    val ncc: VertexRDD[VertexId] = naturalConnectedComponents(eco)
    val ncc_area: Map[(VertexId,VertexId),Long] = nccAreaDistribution(ncc)
    val area_graph: Graph[(EcoUnit,Double),Long] = nccAreaGraph(eco,ncc,ncc_area,size)
    val es_flow: VertexRDD[Double] = esFlow(area_graph,z)
    esGraph(eco,es_flow)
  }




  /**
  @param ival is the initial value for the cummulative sum
  @param es_graph is the biophysical composition of the landscape joined with the es flow
  @param s is this transition's sensitivity with es flow
  @param c is the land cover type required for this transition
  @param f is the function to calculate the propensity of this transition
  @return a vertexRDD with the propensity for a certain transition in each EcoUnit of the graph
  */
  def propensities(ival: Double,
                   es_graph: Graph[(EcoUnit,Double),Long],
                   s: Double,
                   c: String,
                   f: (Double,Double) => Double): ListMap[VertexId,Double] = {
    val sg: Graph[(EcoUnit,Double),Long] = es_graph.subgraph(vpred = (_,(eu,_)) => eu.cover == c)
    val prop: ListMap[VertexId,Double] = ListMap(sg.vertices.mapValues{ (_,(_,es)) =>
       EcoUnit.propensity(es,s,f) }.collect.toSeq.sortWith(_._1 < _._1):_*)
    prop.scanLeft(-1L -> ival)( (pre, k -> v) => k -> v + pre._2 )
    // maybe it is better to scan left on the seq to tail there to then convert to a map
  }

  /**
  @param ival is the initial value for the cummulative sum of the spontaneous propensities
  @param es_graph is the biophysical composition of the landscape joined with the es flow
  @param s is a tuple of the sensitivities for recovery,degradation and fertility loss transitions
  @return a tuple with the maps containing the propensities of each transition type and the last propensity value to continue cumulative sums
  */
  def allSpontaneous(ival: Double,
                     es_graph: Graph[(EcoUnit,Double),Long],
                     s: (Double,Double,Double)): ((ListMap[VertexId,Double],ListMap[VertexId,Double],ListMap[VertexId,Double],ListMap[VertexId,Double]),Double) = {
    val recovery: ListMap[VertexId,Double] = propensities(ival,es_graph,s._1,"Degraded",EcoUnit.increasingPES)
    val degradation: ListMap[VertexId,Double] = propensities(recovery.last._2,es_graph,s._2,"Natural",EcoUnit.decreasingPES)
    val li_floss: ListMap[VertexId,Double] = propensities(degradation.last._2,es_graph,s._3,"Low-Intensity",EcoUnit.decreasingPES)
    val hi_floss: ListMap[VertexId,Double] = propensities(li_floss.last._2,es_graph,s._3,"High-Intensity",EcoUnit.decreasingPES)
    ((recovery._1,degradation._1,li_floss._1,hi_floss._1),hi_floss.last._2)
  }

  def averageESFlow(eco: Graph[EcoUnit,Long],
                    z: Double,
                    size: Int): Double = {
    esGraph(eco,z,size).vertices.reduce{ ((v, a),(_,b)) => (v, a._2 + b._2) }._2 / size.toDouble
  }

  def averageESFlow(eco: Graph[EcoUnit,Long],
                    z: Double,
                    size: Int): Double = {
    esGraph(eco,z,size).vertices.reduce{ ((v, a),(_,b)) => (v, a._2 + b._2) }._2 / size.toDouble
  }

  /**
  Fraction of natural habitat that needs to be removed with uniform probability to
  halve average ecosystem service provision
  */
  def robustnessESFlowOneReplica(average: Double,
                                 eco: Graph[EcoUnit,Long],
                                 z: Double,
                                 size: Int): Double = {

      val thr: Double = average * 0.5
      val vid: VertexId = rnd.shuffle( eco.subgraph(vpred = (_,eu) => eu.cover == "Natural").vertices.collect() ).take(1)._1
      val new_eco: Graph[EcoUnit,Long] = eco.mapValues{ case (v,_) if v == vid => (v, "Degraded") }
      val new_avg: Double = averageESFlow(new_eco,z,size)
      val n: Int = 1

      @tailrec
      def rec(thr: Double,
              current_avg: Double,
              eco: Graph[EcoUnit,Long],
              z: Double,
              size: Int,
              n: Int): Double = {
        if(current_avg <= thr) { n }
        else {
          val new_n: Int = n + 1
          val vid: VertexId = rnd.shuffle( eco.subgraph(vpred = (_,eu) => eu.cover == "Natural").vertices.collect() ).take(1)._1
          val new_eco: Graph[EcoUnit,Long] = eco.mapValues{ case (v,_) if v == vid => (v, "Degraded") }
          val new_avg: Double = averageESFlow(new_eco,z,size)
          rec(thr,new_avg,new_eco,z,size,new_n)
        }
      }
      rec(thr,new_avg,new_eco,z,size,n) / eco.subgraph(vpred = (_,eu) => eu.cover == "Natural").vertices.count.toInt
  }

  def robustnessESFlow(average: Double,
                       eco: Graph[EcoUnit,Long],
                       z: Double,
                       size: Int,
                       n: Int): Double = {
    (0 until n).flatMap( case i => robustnessESFlowOneReplica(average,eco,z,size) ).reduce((a,b) => a + b)/n
  }
}
