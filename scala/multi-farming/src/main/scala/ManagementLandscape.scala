import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph

object ManagementLandscape{
  /**
  @param nm is the number of management units
  @param np is the number of planning units
  @param fs is the fraction of land-sparing management units
  @return a map containign each management unit and their ids
  * TODO: abstract voronoiTesselation torender it more flexible
  **/
  def prepareComposition(nm: Int,
                         np: Int,
                         fs: Double) = ParMap[Long, ManagementUnit] {
    VoronoiUtils.voronoiTesselation(nm,np).groupBy( _._2 ).map{ (key, val) => {
      if (rnd.nextDouble()>fs){
        key.toLong -> (val.values, "Sharing")
      }
      else key.toLong -> (val.values, "Sparing")
      }
    }.toMap.par
  }

  /**
  @param nm is the number of management units
  @param np is the number of planning units
  @param fs is the fraction of land-sparing management units
  @return the composition graph of the management landscape
  */
  def buildComposition(nm: Int,
                       np: Int,
                       fs: Double): Graph[ManagementUnit, Long] = {
    val precomp = prepareComposition(nm,np,fs)
    val sc: SparkContext
    // the units are defined by a vertex id which is the id of the ManagementUnit
    // and a VertexRDD of VertexIds from to the Planning composition graph
    // and representing the PlanningUnits belonging to the ManagementUnit.
    val units: RDD[(VertexId, ManagementUnit)] =
      sc.parallelize( precomp.map{ (_._1, _._2) }.toSeq )

    // for the time being there are no connections between Management Units
    val edges: RDD[Edge[Long]] =
      sc.parallelize(Seq())
    Graph(units,edges)
  }

  /**
  @param comp the management landscape composition
  @param plan the planning landscape
  @param eco the biophyisical landscape
  @return an RDD with the relative conversion probabilities at each management unit. We assume uniform probability among the available units
  */
  def probabilities(comp: Graph[ManagementUnit, Long],
                    plan: Graph[PlanningUnit, Long],
                    eco: Graph[EcoUnit, Long]): VertexRDD[Double] = {
    val sg: Graph[ManagementUnit, Long] = comp.subgraph(vpred = (vid,mu) => mu.isAvailable(plan,eco))
    sg.vertices.mapValues(_ / sg.count)
  }

  /**
  @param ival is the initial value for the propensities
  @param tcp is the total conversion propensity
  @return a ListMap containing the choosing cummulative propensity for each management unit 
  */
  def propensities(ival: Double,
                   tcp: Double,
                   comp: Graph[ManagementUnit, Long],
                   plan: Graph[PlanningUnit, Long],
                   eco: Graph[EcoUnit, Long]): ListMap[VertexId,Double] = {
    val prop: ListMap[VertexId,Double] = ListMap(probabilities(comp,plan,eco).mapValues(_ * tcp).collect.toSeq.sortWith(_._1 < _._1):_*)
    prop.scanLeft((-1L,ival))( (pre, k -> v) => k -> v + pre._2 ).tail
  }

}
