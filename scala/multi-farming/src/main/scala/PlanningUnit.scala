import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph
import scala.math.pow
import scala.math.max

object PlanningUnit {

  /**
  * @param r is the radius of the biophysical landscape
  * @param comp the composition of the planning unit
  * @return an RDD with th IDs of each ecological unit adjacent to the planning unit
  */
  def adjacent(r: Int,
               comp: VertexRDD[VertexId]) = VertexRDD[VertexId]{
    comp.mapValues( ModCo.neighbors(_.toInt,r,1) ).filterNot(comp.exists(_))
  }

  /**
  * @param comp is the composition of the planning unit
  * @param biophy is the composition of the biophysical landscape
  * @return true if the planning unit can be cultivated, false if not
  */
  def isAvailable(comp: VertexRDD[VertexId],
                  biophy: Graph[String,Long]) = Bool{
    comp.exists( biophy.vertices.lookup(_) == "Natural") && comp.forall{ (biophy.vertices.lookup(_) == "Natural" || biophy.vertices.lookup(_) == "Degraded") }
  }

  /**
  * @param nn is the number of neighbors that give wieht to the clustering
  * @param clst is the clustering coefficient
  * @return the clustering weight
  */
  def weightExpression(nn: Int, clst: Double) = Double{
    pow( max(0.1,nn), clst)
  }

}
