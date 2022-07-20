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

object EcoLandscape{

  /**
  @param r is the radius of the biophysical landscape
  @param ecr is the ecological connectivity range
  @return the biophysical composition graph with every unit in a natural state
  */
  def build(r: Int,
            ecr: Int) = Graph[EcoUnit, Long] {
    val sc: SparkContext
    val units: RDD[(VertexId, String)] =
      sc.parallelize( ModCo.apply(r).map{ (_.toLong,EcoUnit("Natural")) }.toSeq )
    val edges: RDD[Edge[Long]] =
      sc.parallelize( ModCo.apply(r).toSet.subsets(2).collect{
        case (pos1,pos2) if ModCo.neighbors(pos1,r,ecr).exists(_ == pos2) =>
          Edge(pos1.toLong,pos2.toLong,0L)
        }
      )
    Graph(units,edges)
  }

  /**
  This function is used to update the landscape after land conversion to agriculture
  @param uids are the vertexId of the units to update
  @param c is the new land cover
  @param eco is the biophysical landscape composition
  @return the updated composition graph
  */
  def updated(vids: VertexRDD[VertexId],
              c: String
              eco: Graph[EcoUnit, Long]) = Graph[EcoUnit, Long] {
    val upd_vertices: VertexRDD = eco.vertices.mapValues{ case (vid, _) =>
      if uids.contains(vid) => EcoUnit(c) }
      // the new cover is the same for every unit when multiple units are updated it is because of conversion to agriculture
    Graph( upd_vertices, eco.edges )
  }

  /**
  This function is used to update the landsacpe after a spontaneous transition
  @param uid is the vertexId of the unit to update
  */
  def updated(uid: VertexId,
              c: String
              eco: Graph[EcoUnit, Long]) = Graph[EcoUnit, Long] {
    val upd_vertices: VertexRDD = eco.vertices.mapValues{ case (vid, _) =>
      if (uid == vid) => EcoUnit(c) }
    Graph( upd_vertices, eco.edges )
  }

  /**
  @param fagr is fraction of agricultural units in the initial biophysical landscape
  @param fdeg is fraction of degraded units in the initial biophysical landscape
  @return a biophysical landscape initialized according to the simulation parameter values
  */
  def init(eco: Graph[EcoUnit,Long],
           pln: Graph[PlnUnit,Long],
           mng: Graph[MngUnit,Long],
           size: Int,
           z: Double,
           fagr: Double,
           fdeg: Double): Graph[EcoUnit,Long] = {

    val n_agr: Int = size*fagr.toInt
    val n_deg: Int = size*fdeg.toInt

    @tailrec
    def rec(eco: Graph[EcoUnit,Long],
            pln: Graph[PlnUnit,Long],
            mng: Graph[MngUnit,Long],
            size: Int,
            z: Double,
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
        rec(upd_eco, plan, mng, size, z, n_remaining._1, n_remaining._2)
      }
    }
    rec(eco, pln, mng, size, z, n_agr, n_deg)
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
    val es_graph: Graph[(EcoUnit,Double),Long] = EcosystemServices.esGraphDirect(eco,es_flow)
    propensities(0.0,es_graph,1.0,"Natural",EcoUnit.decreasingPES)
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
  @param eco is the biophysical landscape's composition graph
  @return the vertices in the connected components graph
  */
  def naturalConnectedComponents(eco: Graph[EcoUnit, Long]) = VertexRDD[VertexId]{
    val natural = eco.subgraph(vpred = (vid,eu) => eu.cover == "Natural")
    val ncc = natural.connectedComponents().vertices
  }

  /**
  @param ncc is the natural connected components, VertexRDD is over the EcoUnits and VertexId refers to the component Id
  @return a map with the number of units in each component
  */
  def nccAreaDistribution(ncc: VertexRDD[VertexId]) = Map[(VertexId,VertexId), Long] {
    ncc.countByValue()
  }

  /**
  @param size is the total number of EcoUnits in the landscape
  @param ncc_area is a map with the area of each natural connected component
  @return a biophysical landscape graph with information on the area of the ncc of each node
  */
  def nccAreaGraph(eco: Graph[EcoUnit, Long],
                   ncc: VertexRDD[VertexId],
                   ncc_area: Map[(VertexId,VertexId), Long],
                   size: Double): Graph[(EcoUnit,Double), Long] = {
    val area_vertices: VertexRDD[Double] = ncc.mapValues{case (uid,cid) => ncc_area.get((uid,cid)).toDouble }
    // Create a graph where each node attribute is the normalized area of the
    // natural component the ecological unit with such id belongs to. If the
    // vertex is not a natural cell, then put 0.0 as attribute.
    eco.outerJoinVertices(area_vertices){ (id, _, av_opt) =>
      av_opt match {
        case Some(vertex_area) => (_,vertex_area/size)
        case None => (_, 0.0)
      }
    }
  }


  /**
  @param a is the area of the natural component
  @param z is the scaling exponent of the ecosystem services area relationship
  @return the value of ecosystem service provision for a component of area a
  */
  def esAreaRelation(a: Double,
                     z: Double): Double = {
    pow(a,z)
  }

  /**
  @param area_graph is the biophysical composition of the landscape joined with the ncc area
  @param z is the scaling exponent of the ecosystem services area relationship
  @return a VertexRDD with the ecosystem service inflow as an attribute
  */
  def esFlow(area_graph: Graph[(EcoUnit,Double),Long],
             z: Double): VertexRDD[Double] = {
    area_graph.aggregateMessages[(Int,Double)](
      triplet => {
        if (triplet.srcAttr._1.cover == "Natural") {
          // the second attribute is the component's area
          triplet.sendToDst((1,esAreaRelation(triplet.srcAttr._2, z)))
        }
        else triplet.sendToDst((1,0.0))
      },
      (a,b) => (a._1 + b._1, a._2 + b._2)
    ).mapValues( (id,val) => val._2/val._1 )
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
  This function directly returns the ecosystem services graph performing the
  intermediate steps inside
  */
  def esGraphDirect(eco: Graph[EcoUnit,Long],
                    z: Double,
                    size: Int): Graph[(EcoUnit,Double),Long] = {
    val ncc: VertexRDD[VertexId] = naturalConnectedComponents(eco)
    val ncc_area: Map[(VertexId,VertexId),Long] = nccAreaDistribution(ncc)
    val area_graph: Graph[(EcoUnit,Double),Long] = nccAreaGraph(eco,ncc,ncc_area,size)
    val es_flow: VertexRDD[Double] = esFlow(area_graph,z)
    esGraph(eco,es_flow)
  }

  /**
  @param es_graph is the biophysical composition of the landscape joined with the es flow
  @param y1
  @param y2
  @return the total amount of resources produced in the low-intensity units
  */
  def lowIntResources(es_graph: Graph[(EcoUnit,Double),Long],
                      y1: Double,
                      y2: Double): Double = {
    // this function creates a subgraph and then traverses it, another option would be
    // to traverse the whole graph and match at each node agains the cover to calculate production
    // i am not sure what is the optimal. Creating a subgraph seems expensive, but at the
    // same time the subgraph function might be optimized within spark
    val low_intensity = es_graph.subgraph(vpred = (vid,(eu,_)) => eu.cover == "Low-Intensity")
    low_intensity.vertices.mapValues{ case (eu, es) => EcoUnit.lowIntResEquation(y1,y2,es) }.reduce( _+_ )
  }

  /**
  @return the total amount of resources produced in the high-intensity units
  */
  def highIntResources(es_graph: Graph[(EcoUnit,Double),Long]): Double = {
    // this functions follows the same approach as the low intensity one. However,
    // due to non-dimensionalization in this model the total high intensity production
    // is just the number of high-intensity units, maybe it is better to just do that
    // trade off between clarity of code and execution time
    val high_intensity = es_graph.subgraph(vpred = (vid,(eu,_)) => eu.cover == "High-Intensity")
    high_intensity.vertices.mapValues{ case (eu, es) => EcoUnit.highIntResEquation() }.reduce( _+_ )
  }

  /**
  @return the total amount of resources produced in the landscape
  */
  def resources(es_graph: Graph[(EcoUnit,Double),Long],
                y1: Double,
                y2: Double): Double = {
    lowIntResources(es_graph,y1,y2) + highIntResources(es_graph)
  }

  /**
  This function is used in the world initialization
  @return the total amount of resources produced in the landscape
  */
  def resources(eco: Graph[EcoUnit,Long],
                z: Double,
                size: Int,
                y1: Double,
                y2: Double): Double = {
    val es_graph: Graph[(EcoUnit,Double),Long] = esGraphDirect(eco,z,size)
    resources(es_graph,y1,y2)
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
}
