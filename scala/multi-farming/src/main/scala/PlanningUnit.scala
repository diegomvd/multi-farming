import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph
import scala.math.pow
import scala.math.max

case class PlanningUnit(comp: VertexRDD[VertexId]){

  def adjacent(r: Int){
    PlanningUnit.adjacent(r,this.comp)
  }

  def isAvailable(eco: Graph[EcoUnit,Long]){
    PlanningUnit.isAvailable(this.comp,eco)
  }
}

object PlanningUnit {

  /**
  @paramr is the radius of the biophysical landscape
  @paramcomp the composition of the planning unit
  @return an RDD with th IDs of each ecological unit adjacent to the planning unit
  */
  def adjacent(r: Int,
               comp: VertexRDD[VertexId]) = VertexRDD[VertexId]{
    comp.mapValues( ModCo.neighbors(_.toInt,r,1) ).filterNot(comp.exists(_))
  }

  /**
  @paramcomp is the composition of the planning unit
  @parameco is the composition of the biophysical landscape
  @return true if the planning unit can be cultivated, false if not
  */
  def isAvailable(comp: VertexRDD[VertexId],
                  eco: Graph[EcoUnit,Long]) = Bool{
    comp.exists( eco.vertices.lookup(_).cover == "Natural") && comp.forall{ (eco.vertices.lookup(_).cover == "Natural" || eco.vertices.lookup(_).cover == "Degraded") }
  }

  /**
  @paramnn is the number of neighbors that give wieht to the clustering
  @paramclst is the clustering coefficient
  @return the clustering weight
  */
  def weightExpression(nn: Int, clst: Double) = Double{
    pow( max(0.1,nn), clst)
  }

}
