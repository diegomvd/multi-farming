case class PlanningLandscape(composition: ParMap[Int, PlanningUnit], base: BioPhyLandscape ){

  def available = ParVector[Int]{
    this.composition.collect{ case (id, un) if un.isAvailable(this.base) => id }.toVector.par
  }

  def conversionWeights(units: Set[Int], strategy: String ) = ParMap[Int,Double] {
    val propensity = this.composition.collect{
      case units.contains(_._1) => _._1 -> _._2.conversionWeight(this.base,strategy)
    }.toMap.par
    val total_propensity = propensity.sum[Int :> (Int,Double)](_._2 + _._2)
    propensity.map( _._1 -> _._2/total_propensity )
  }

}

object PlanningLandscape{

  /**
  After performing a Voronoi tesselation over EcoUnits positions, units
  are grouped by voronoi cell id and and a ManagementUnit is initialized per
  set of EcoUnits in the same voronoi cell.
  Management unit composition should be a set of ecounits to facilitate look up
  **/
  def buildComposition(n_units: Int, radius: Int) = ParMap[Int, PlanningUnit] {
    VoronoiUtils.voronoiTesselation(n_units,radius).groupBy( _._2 ).map{
      (key, val) => key -> PlanningUnit(key, val.values.toSet, Vector())
    }.toMap.par
  }

  /**
  After building composition provide structure by updating the neighbors of
  each planning unit
  **/
  def buildStructure(ParMap[Int, PlanningUnit]) = ParMap[Int,PlanningUnit]{
    // here update the neighbor field of each planning unit
  }

  def buildPlanningLandscape(n_units: Int, base: BioPhyLandscape) = PlanningLandscape {
    PlanningLandscape(PlanningLandscape.buildComposition(n_units, base.radius()), base)
  }

  def ecoNeighbors(radius: Int, unit_composition: Set[ModuloCoord]) = Vector[ModuloCoord]{
    unit_composition.map( _.manhattanNeighbors(radius,1) ).toVector
  }

  def neighbors(radius: Int, unit_id: Int, composition: ParMap[Int, Set[ModuloCoord]]) = Vector[Int]{
    PlanningLandscape.ecoNeighbors(radius, composition.get(unit_id)).map{ en => composition.find( _._2.exists(_.contains(en)))._1 }.toSet.toVector
  }

  def isAvailable(unit_composition: Set[ModuloCoord], biophy_composition: ParMap[ModuloCoord, EcoUnit] ){
    unit_composition.forall{ biophy_composition.get(_).isNotCultivated() }
  }
}
