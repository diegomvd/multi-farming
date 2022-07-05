import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph

case class ManagementUnit(comp: VertexRDD[VertexId], stg: String){

  def isAvailable(plan: Graph[PlanningUnit,Long],
                  eco: Graph[EcoUnit,Long]): Bool = {
    ManagementUnit.isAvailable(this.comp,plan,eco)
  }

  def conversionPropensity(plan: Graph[PlanningUnit,Long],
                           eco: Graph[EcoUnit,Long],
                           tcp: Double): VertexRDD[Double] = {
    ManagementUnit.conversionPropensity(this.comp,plan,eco,this.stg,tcp)
  }
}

object ManagementUnit{

  /**
  * @param comp is the set of planning unit ids belonging to this managament unit: a vertex of the management graph
  * @param plan is the planning landscape
  * @param eco is the biophysical landscape
  * @return true if the management unit is available, false if not
  */
  def isAvailable(comp: VertexRDD[VertexId],
                  plan: Graph[PlanningUnit,Long],
                  eco: Graph[EcoUnit,Long]) : Bool = {
    comp.exists{  plan.lookup(_).comp.isAvailable(eco) }
  }

  /**
  * @param comp is the composition of the management unit
  * @param plan is the planning landscape composition graph
  * @param eco is the biophysical landscape composition graph
  * @param stg is the management strategy of the unit
  * @param tcp is the total conversion propensity
  * @return a VertexRDD with the propensity associated to each PU
  */
  def conversionPropensity(comp: VertexRDD[VertexId],
                           plan: Graph[PlanningUnit,Long],
                           eco: Graph[EcoUnit,Long],
                           stg: String,
                           tcp: Double): VertexRDD[Double] = {

    val pun = PlanningLandscape.extendedSubGraph(plan,comp)

    stg match {
      case "Sparing" => val w = PlanningLandscape.unavailableNeighbors(pun,eco).mapValues( PlanningUnit.weightExpression(_,3.0) )
      case "Sharing" => val w = PlanningLandscape.availableNeighbors(pun,eco).mapValues( PlanningUnit.weightExpression(_,3.0) )
    }

    val w_tot = w.reduce(_+_)
    w_tot match {
      case 0.0 => w
      case _ => w.mapValues(_ / w_tot * tcp)
    }
  }
}
