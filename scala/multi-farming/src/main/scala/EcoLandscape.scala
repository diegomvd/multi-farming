import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph
import scala.math.pow

object EcoLandscape{

  /**
  * @param r is the radius of the biophysical landscape
  * @param ecr is the ecological connectivity range
  * @return a biophysical composition with every unit in a natural state
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
  * @param vids are the vertexId of the units to update
  * @param cover is the new land cover
  * @param eco is the biophysical landscape composition
  * @return the updated composition
  */
  def updated(vids: VertexRDD[VertexId],
              cover: String
              eco: Graph[EcoUnit, Long]) = Graph[EcoUnit, Long] {
    val upd_vertices: VertexRDD = eco.vertices.mapValues{ case (vid, _) =>
      if vids.contains(vid) => EcoUnit(cover) } // note that the new cover is the same for every unit
    Graph( upd_vertices, eco.edges )
  }

  /**
  * @param uid is the vertexId of the unit
  * @param cover is the new land cover
  * @param eco is the biophysical landscape composition
  * @return the updated composition
  */
  def updated(uid: VertexId,
              cover: String
              eco: Graph[EcoUnit, Long]) = Graph[EcoUnit, Long] {
    val upd_vertices: VertexRDD = eco.vertices.mapValues{ case (vid, _) =>
      if (uid == vid) => EcoUnit(cover) } // note that the new cover is the same for every unit
    Graph( upd_vertices, eco.edges )
  }

  /**
  * @param eco is the biophysical landscape's composition
  * @return the vertices in the connected components graph
  */
  def naturalConnectedComponents(eco: Graph[EcoUnit, Long]) = VertexRDD[VertexId]{
    val natural = eco.subgraph(vpred = (vid,eu) => eu.cover == "Natural")
    val ncc = natural.connectedComponents().vertices
  }

  /**
  * @param ncc is the natural connected components, VertexRDD is over the EcoUnits
  * and VertexId refers to the component id
  * @return a map with the number of units in each component
  */
  def nccAreaDistribution(ncc: VertexRDD[VertexId]) = Map[(VertexId,VertexId), Long] {
    ncc.countByValue()
  }

  /**
  * @param size is the total number of EcoUnits in the landscape
  * @return a biophysical landscape graph with information on the area of the ncc of each node
  */
  def nccAreaGraph(eco: Graph[EcoUnit, Long],
                   ncc: VertexRDD[VertexId],
                   ncc_area: Map[(VertexId,VertexId), Long],
                   size: Double): Graph[(EcoUnit,Double), Long] = {
    val area_vertices: VertexRDD[Double] = ncc.map{ ncc_area.get((_,_)).toDouble }

    // join the area vertices and drop the ecounit
    eco.outerJoinVertices(area_vertices){ (id, _, av_opt) =>
      av_opt match {
        case Some(vertex_area) => (_,vertex_area/size)
        case None => (_, 0.0)
      }
    }
  }

  /**
  * @param eco_join is the biophysical composition of the landscape joined with the ncc area
  * @param z is the ES-area scaling exponent
  * @return an RDD with each unit and the ES flow they receive
  */
  def esFlow(eco_join: Graph[(EcoUnit,Double),Long],
             z: Double): VertexRDD[Double] = {
    eco_join.aggregateMessages[(Int,Double)](
      triplet => {
        if (triplet.srcAttr._1.cover == "Natural") {
          // the second attribute is the component's area
          triplet.sendToDst((1,pow(triplet.srcAttr._2, z)))
        }
        else triplet.sendToDst((1,0.0))
      },
      (a,b) => (a._1 + b._1, a._2 + b._2)
    ).mapValues( (id,val) => val._2/val._1 )
  }

  /**
  * @param eco is the biophysical composition of the landscape
  * @param es is the ecosystem service flow in each EcoUnit
  * @return a graph joining the ecounits with the ES flow they receive
  */
  def esGraph(eco: Graph[EcoUnit,Long],
              es: VertexRDD[Double]): Graph[EcoUnit,Long] = {
     eco.outerJoinVertices(es){ (vid, eu, es_opt) =>
       es_opt match {
         case Some(es) => (eu, es)
         case None => (eu, 0.0)
       }
     }
  }

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
  * @param eco_join is the biophysical composition of the landscape joined with the es flow
  * @param y1
  * @param y2
  * @return the total amount of resources produced in the landscape
  */
  def resources(eco_join: Graph[(EcoUnit,Double),Long],
                y1: Double,
                y2: Double): Double = {
    eco_join.vertices.mapValues{ case (eu, es) => f(es,y1,y2) }.reduce( _+_ )
  }

  def resourcesDirect(eco: Graph[EcoUnit,Long],
                      z: Double,
                      size: Int,
                      y1: Double,
                      y2: Double): Double = {
    val es_graph: Graph[(EcoUnit,Double),Long] = esGraphDirect(eco,z,size)
    es_graph.vertices.mapValues{ case (eu, es) => f(es,y1,y2) }.reduce( _+_ )
  }

  /**
  * @param ival is the initial value for the cummulative sum
  * @param eco_join is the biophysical composition of the landscape joined with the es flow
  * @param s is this transition's sensitivity with es flow
  * @param c is the land cover type required for this transition
  * @param f is the function to calculate the propensity of this transition
  * @return a vertexRDD with the propensity for a certain transition in each EcoUnit of the graph
  */
  def propensities(ival: Double,
                   eco_join: Graph[(EcoUnit,Double),Long],
                   s: Double,
                   c: String,
                   f: (Double,Double,Bool) => Double): VertexRDD[Double] = {
    val prop: VectorMap[VertexId,Double] = es_graph.vertices.mapValues{ (id, (eu, esf)) =>
       EcoUnit.propensity(esf, s, eu.matchCover(c), f) }.collect.toSeq.groupMapReduce(_._1)(_._2)(_+_).sortBy(...)
    prop.scanLeft(_+_)
  }

  /**
  This is way sub optimal as th es graph would be calculated again for each type of propensity
  */
  def propensitiesDirect(eco: Graph[EcoUnit,Long],
                         z: Double,
                         size: Int,
                         s: Double,
                         c: String,
                         f: (Double,Double,Bool) => Double): VertexRDD[Double] = {
    val es_graph: Graph[(EcoUnit,Double),Long] = esGraphDirect(eco,es_flow)
    propensities(es_graph,s,c,f)
  }

  /**
  * @param eco is the biophysical landscape graph
  * @param z is the ecosystem services - area scaling
  * @param size is the total number of units in the biophysical landscape
  * @return the degradation propensity for the initialization
  */
  def degradationPropensityInit(eco: Graph[EcoUnit,Long],
                                z: Double,
                                size: Int): VertexRDD[Double] = {
    val es_graph: Graph[(EcoUnit,Double),Long] = esGraphDirect(eco,es_flow)
    propensities(es_graph,1.0,"Natural",EcoUnit.degradationEquation)
  }

  def initAgriculturalUnit(eco: Graph[EcoUnit,Long],
                           plan: Graph[PlanningUnit,Long],
                           mng: Graph[ManagementUnit,Long]
                           tcp: Double): Graph[EcoUnit,Long] = {
    // this might be wrong because 1.0 should be the total propensity
    val x_rnd: Double = rnd.nextDouble( tcp )
    World.applyConversionEvent(x_rnd,eco,plan,mng,tcp)
  }

  def initDegradedUnit(eco: Graph[EcoUnit,Long],
                       z: Double,
                       size: Int){
    val prop: VertexRDD[Double] = degradationPropensityInit(eco,z,size)
    val x_rnd: Double = rnd.nextDouble( prop.reduce(_+_) )
    World.applySpontaneousEvent(x_rnd,prop,eco,"Degraded")
  }

  def initUpdateRemaining(n: (Int,Int),
                          event: String): (Int,Int) = {
    event match{
      case "Conversion" =>{
        val upd_n_deg = n._2
        if n._1>0 { val upd_n_agr = n._1 - 1 }
        else {val upd_n_agr = n._1}
      }
      case "Degradation" =>{
        val upd_n_agr = n._1
        if n._2>0 { val upd_n_deg = n._2 - 1 }
        else {val upd_n_deg = n._1}
      }
    }
    (upd_n_agr,upd_n_deg)
  }

  def init(eco: Graph[EcoUnit,Long],
           plan: Graph[PlanningUnit,Long],
           mng: Graph[ManagementUnit,Long],
           size: Int,
           z: Double,
           fagr: Double,
           fdeg: Double): Graph[EcoUnit,Long] = {

    val n_agr: Int = size*fagr.toInt
    val n_deg: Int = size*fdeg.toInt

    @tailrec
    def rec(eco: Graph[EcoUnit,Long],
            plan: Graph[PlanningUnit,Long],
            mng: Graph[ManagementUnit,Long],
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
            val n_remaining: (Int,Int) = initUpdateRemaining(n_agr,n_deg,"Conversion")
            val upd_eco: Graph[EcoUnit,Long] = initAgriculturalUnit(eco,plan,mng,1.0)
          }
          case n_rnd if n_rnd<n_deg => {
            val n_remaining: (Int,Int) = initUpdateRemaining(n_agr,n_deg,"Degradation")
            val upd_eco: Graph[EcoUnit,Long] = initDegradedUnit(eco,z,size)
          }
        }
        rec(upd_eco, plan, mng, size, z, n_remaining._1, n_remaining._2)
      }
    }
  }

  def allSpontaneous(ival: Double,
                     eco_join: Graph[(EcoUnit,Double),Long],
                     s: (Double,Double,Double)): (VertexRDD[Double],VertexRDD[Double],VertexRDD[Double],Double) = {
    val recovery: (VertexRDD[Double], Double) = propensities(ival,eco_join,s._1)
    val degradation: (VertexRDD[Double], Double) = propensities(recovery._2,eco_join,s._2)
    val fertility_loss: (VertexRDD[Double], Double) = propensities(degradation._2,eco_join,s._3)
    (recovery._1,degradation._1,fertility_loss._1,fertility_loss._3)
  }

}
