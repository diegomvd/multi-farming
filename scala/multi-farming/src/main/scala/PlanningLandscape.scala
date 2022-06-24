case class PlanningLandscape(composition: ParMap[Int, PlanningUnit]){

  def availableUnits(biocomp: ParMap[ModuloCoord,EcoUnit]) = ParVector[Int]{
    this.composition.collect{ case (id, un) if un.isAvailable(biocomp) => id }.toVector.par
  }

  def conversionWeights(units: Set[Int],
                        strategy: String,
                        biocomp: ParMap[ModuloCoord,EcoUnit]) = ParMap[Int,Double] {
    val propensity = this.composition.collect{ case units.contains(_._1) =>
       _._1 -> _._2.conversionWeight(biocomp,strategy)
    }.toMap.par
    val total_propensity = propensity.sum[Int :> (Int,Double)](_._2 + _._2)
    propensity.map( _._1 -> _._2/total_propensity )
  }

}

object PlanningLandscape{

  /**
  * This function is a preliminary computation of the planning landscape's
  * composition.
  *  TODO: abstract voronoiTesselation
  *  @param n_units is the number of planning units to create
  *  @param radius is the radius of the biophysical landscape
  *  @return a map storing the id and composition of each planning unit
  */
  def prepareComposition(n_units: Int, radius: Int) = ParMap[Int, Set[ModuloCoord]] {
    VoronoiUtils.voronoiTesselation(n_units,radius).groupBy( _._2 ).map{ (key, val) =>
      key -> val.values.toSet
    }.toMap.par
  }

  /**
  * @param id is the planning unit id
  * @param radius is the radius of the biophysical landscape
  * @param comp is the composition of the planning landscape
  * @return a vector containing the ids of the planning units in the neighborhood
  */
  def ecoNeighbors(id: Int,
                   radius: Int,
                   comp: ParMap[Int, Set[ModuloCoord]]) = Vector[ModuloCoord] {
    PlanningUnit.ecoNeighbors(radius,comp.get(id))
  }

  /**
  * @param id is the planning unit id
  * @param radius is the radius of the biophysical landscape
  * @param comp is the composition of the planning landscape
  * @return a vector containing the ids of the planning units in the neighborhood
  */
  def planningNeighbors(id: Int,
                        radius: Int,
                        comp: ParMap[Int, Set[ModuloCoord]]) = Vector[Int] {
    PlanningUnit.ecoNeighbors(radius,comp.get(id)).map{ coord => comp.find( _._2.exists.(_.contains(coord)))._1 }.toSet.toVector
  }

  /**
  * @param n_units is the number of planning units to create
  * @param radius is the radius of the biophysical landscape
  * @return a vector containing the composition of the planning landscape
  */
  def buildComposition(n_units: Int, radius: Int) = ParMap[Int,PlanningUnit] {
    val comp = prepareComposition(n_units,radius)
    comp.map{ (id,c) =>
      id -> PlanningUnit(id, c.get(id), ecoNeighbors(id,radius,comp), planningNeighbors(id,radius,comp))
    }.toMap.par
  }

  /**
  * @param n_units is the number of planning units to create
  * @param radius is the radius of the biophysical landscape
  * @return the planning landscape
  */
  def apply(n_units: Int, radius: Int) = PlanningLandscape {
    PlanningLandscape( buildComposition( n_units, radius ) )
  }

}
