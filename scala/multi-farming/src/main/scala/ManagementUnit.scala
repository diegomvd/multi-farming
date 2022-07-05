case class ManagementUnit(id: Int, comp: Set[Int], strategy: String){

  def id = this.id
  def composition = this.comp
  def strategy = this.strategy

  def isAvailable(landscape: PlanningLandscape): Bool =
    ManagementUnit.isAvailable(this.comp,landscape)

  def conversionPropensity(landscape: PlanningLandscape,
                           biocomp: ParMap[ModuloCoord,EcoUnit],
                           tcp: Double): ParMap[Int,Double] =
    ManagementUnit.conversionPropensity(this.comp,this.strategy,landscape,biocomp,tcp)

}

object ManagementUnit{

  /**
  * @param comp is the set of planning unit ids belonging to this managament unit: a vertex of the management graph
  * @param plan is the planning landscape
  * @param biophy is the biophysical landscape
  * @return true if the management unit is available, false if not
  */
  def isAvailable(comp: VertexRDD[VertexId],
                  plan: PlanningLandscape,
                  biophy: BioPhysicalLandscape) = Bool {
    comp.exists{
      // this gets the planning unit atribute which is a vertexRDD with the vertexID of the EcoUnits composing it
      PlanningUnit.isAvailable( plan.comp.lookup(_), biophy.comp )
    }
  }

  /**
  * @param comp is the set of planning unit ids belonging to this managament unit
  * @param stg is the land-use management strategy of this unit
  * @param plan is the planning landscape
  * @param biophy is the composition of the biophysical landscape
  * @param tcp is the total conversion propensity
  * @return a map with the ids of each planning unit in this management unit associated with their conversion propensity
  */
  def conversionPropensity(select: VertexRDD[VertexId],
                           stg: String,
                           plan: PlanningLandscape,
                           biophy: Graph[String,Long],
                           tcp: Double) = VertexRDD[Double] {
    plan.conversionWeights(select, biophy, stg).mapValues( _ * tcp )
  }
}
