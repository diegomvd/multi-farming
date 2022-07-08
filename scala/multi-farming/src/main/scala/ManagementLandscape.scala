import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph

object ManagementLandscape{
  /**
  * @param nm is the number of management units
  * @param np is the number of planning units
  * @param fs is the fraction of land-sparing management units
  * @return a map containign each management unit and their ids
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
  * @param nm is the number of management units
  * @param np is the number of planning units
  * @param fs is the fraction of land-sparing management units
  * @return the composition graph of the management landscape
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
  * @param comp the management landscape composition
  * @param plan the planning landscape
  * @param tcp the total conversion propensity
  * @return an RDD with the conversion propensity of each management unit
  * TODO: create a normalize function
  */
  def conversionPropensity(comp: Graph[ManagementUnit, Long],
                           plan: Graph[PlanningUnit, Long],
                           eco: Graph[EcoUnit, Long],
                           tcp: Double) : VertexRDD[Double] = {
    // sends 1 if there is at least one available PU in the MU: thus this means
    // traversing the graph and lookin if MU is available
    val prop = comp.vertices.mapValues{ (vid, mu) =>
      if mu.isAvailable(plan,eco){
        1.0
      }
      else 0.0
    }
    val sum = prop.reduce(_+_)
    sum match{
      case 0.0 => prop
      case _ => prop.mapValues(_ / sum * tcp)
    }
  }

  def totalConversionPropensity()=

}
