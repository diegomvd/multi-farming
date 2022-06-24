case class ManagementLandscape(comp: ParMap[Int, ManagementUnit]){

  def conversionPropensity(landscape: PlanningLandscape,
                           tcp: Double) = ParMap[Int,Double] {
    ManagementLandscape.conversionPropensity(this.comp,landscape,tcp)
  }
}

object ManagementLandscape{
  /**
  * @param n_mng is the number of management units
  * @param n_plan is the number of planning units
  * @param fs is the fraction of land-sparing management units
  * @return a map containign each management unit and their ids
  * TODO: abstract voronoiTesselation torender it more flexible
  **/
  def buildComposition(n_mng: Int,
                       n_plan: Int,
                       fs: Double) = ParMap[Int, ManagementUnit] {
    VoronoiUtils.voronoiTesselation(n_mng,n_plan).groupBy( _._2 ).map{ (key, val) => {
      if (rnd.nextDouble()>fs) key -> ManagementUnit(key, val.values.toSet, "Sharing")
      else key -> ManagementUnit(key, val.values.toSet, "Sparing")
      }
    }.toMap.par
  }

  def apply(n_mng: Int, n_plan: Int, fs: Double) = ManagementLandscape {
    ManagementLandscape( buildComposition(n_mng,n_plan,fs) )
  }

  /**
  * @param comp the management landscape composition
  * @param landscape the planning landscape
  * @param tcp the total conversion propensity
  * @return the conversion propensity of each management unit stored in a map
  * TODO: create a normalize function 
  */
  def conversionPropensity(comp: ParMap[Int, ManagementUnit],
                           landscape: PlanningLandscape,
                           tcp: Double) = ParMap[Int,Double] {
    val propensity = this.composition.map{ (id, un) =>
      if un.isAvailable(landscape) id -> 1.0
      else id -> 0.0
    }.toMap.par
    val sum = propensity.sum[Double >: (Int,Double)](_._2 + _._2)
    propensity.map( _._1 -> _._2/sum*tcp ).toMap.par
  }

}
