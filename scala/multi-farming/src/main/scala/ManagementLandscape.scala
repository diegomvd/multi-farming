case class ManagementLandscape(composition: ParMap[Int, ManagementUnit], base: PlanningLandscape){

  def conversionPropensity(total_propensity: Double) = ParMap[Int,Double] {
    val propensity = this.composition.map{
      case (id, un) if un.isAvailable(this.base) => id -> total
      case _ => id -> 0.0
    }.toMap.par
    val total_propensity = propensity.sum[Double >: (Int,Double)](_._2 + _._2)
    propensity.map( _._1 -> _._2/total_propensity ).toMap.par
  }


}

object ManagementLandscape{
  /**
  The idea is also to perform a Voronoi tesselation but now over the
  ManagementUnits. This only needs a neighborhood relationship to be defined
  between ManagementUnits. For this to work I need to generalize the types
  taken by VoronoiUtils.
  **/
  def buildComposition(n_regions: Int, n_units: Int, fraction_sparing: Double) = ParMap[Int, ManagementUnit] {
    VoronoiUtils.voronoiTesselation(n_regions,n_units).groupBy( _._2 ).map{
      (key, val) => { if (rnd.nextDouble()>fraction_sparing) key -> ManagementUnit(key, val.values.toSet, "Sharing")
                      else key -> ManagementUnit(key, val.values.toSet, "Sparing")
                    }
    }.toMap.par
  }


}
