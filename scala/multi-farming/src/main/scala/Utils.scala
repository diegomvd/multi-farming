object VoronoiUtils{

  def seedVoronoiTesselation(n_seeds: Int, radius: Int) = ParMap[ModuloCoord, Int]{
    val area = 3 * radius * radius + 3 * radius + 1
    (0 until n_seeds).flatMap(id => id -> ModuloCoord(rnd.nextInt(area))).toMap.par
  }

  def voronoiProbability(radius: Int, cells: ParMap[ModuloCoord, Int]) = ParMap[ModuloCoord, Double] {
    val non_norm_prob = ModuloCoord.apply(radius).map{
                          case pos if cells.exists((p,_)==(pos,_)) => pos -> 0.0
                          case pos if pos.manhattanNeighbors(radius,1).exists(p => cells.exists( _.contains(p) ) ) => pos -> 1.0
                          case other => pos -> 0.0
                        }.toMap.par
    val total_prob = non_norm_prob.sum[Int >: (ModuloCoord,Int)]( _._2 + _._2  )
    non_norm_prob.map( case (pos, x) => pos -> x/total_prob).toMap.par
  }

  @annotation.tailrec
  def voronoiRadialGrowth(radius: Int, cells: ParMap[ModuloCoord, Int]) = ParMap[ModuloCoord, Int]{
    val area = 3 * radius * radius + 3 * radius + 1
    if (cells.size() >= area) cells
    else{
      val pos = S3Utils.positionSelector( VoronoiUtils.voronoiProbability(radius, cells) )
      val cell = S3Utils.eventSelector( radius, pos, cells )
      val new_cells = cells.map( case (_,id) => pos -> id ).toMap.par
      VoronoiUtils.voronoiRadialGrowth( radius, pos, new_cells)
    }
  }

  def voronoiTesselation(n_seeds: Int, radius: Int) = ParMap[ModuloCoord,Int]{
    VoronoiUtils.voronoiRadialGrowth(radius, VoronoiUtils.seedVoronoiTesselation(n_seeds,radius))
  }

object S3Utils{ // utility functions for spatial stochastic simulations

  // could be transformed to ParMap[Int,Double] to make it more general
  def positionSelector(prob: ParMap[ModuloCoord, Double]) = ModuloCoord {
    // one liner to get the position by ordering the map into a vector map
    val x_rand = rnd.nextDouble(1.0)
    VectorMap(prob.toSeq.sortBy(_._1.m):_*).scanLeft[(ModuloCoord,Double)]((ModuloCoord(-1),0.0)){case ((_,acc),(pos,x)) => (pos, acc + x)}.find((_,x) => x_rand <= x)._1
  }

  def eventSelector(radius: Int, pos: ModuloCoord, events: ParMap[ModuloCoord, Int]) = Int {
    val potential_events = pos.manhattanNeighbors(radius,1).foldLeft(Vector[Int]())(case (allocated, p) => allocated :+ cells.get(p)).filter( _ != None ).toVector
    potential_events(rnd.nextInt(potential_events.size()))
  }

  def selectEvent(recovery: ParMap[ModuloCoord,Double], degradation: ParMap[ModuloCoord,Double], fertility_loss: ParMap[ModuloCoord,Double], management: ParMap[Int,Double], birth: Double, death: Double) = {

    // assuming all the propensities come cummulative and sorted
    val x_rand = rnd.nextDouble(recovery.last + degradation.last + fertility_loss.last + management.last + population.last)

    /**
    Better to subdivide by spontaneous, management, and population.
    Go by cases and choose.
    Then call specific function for each category taking the total
    **/

    S3Utils.selectCategory(recovery.last,degradation.last,fertility_loss.last,management.last,birth,death,x_rand) match {
      case "recovery" => "recovery" -> selectPosition(recovery,0.0,x_rand)
      case "degradation" => "degradation" -> selectPosition(degradation,recovery.last,x_rand)
      case "fertility_loss" => "fertility_loss" -> selectPosition(fertility_loss,recovery.last+degradation.last,x_rand)
      case "management" => "management" -> selectManagementUnit(management,spontaneous.last,x_rand)
      case "birth" => "death" -> selectPopulation(population,spontaneous.last+management.last,x_rand)
    }
  }

  def selectPosition(propensity: ParMap[ModuloCoord,Double], add: Double, x_rand: Double) = ModuloCoord {
    S3Utils.positionSelector(propensity.map( _._1 -> _._2 + add ).toMap.par)
  }

  def selectManagementUnit(propensity: ParMap[Int,Double], add: Double, x_rand: Double) = Int {
    S3Utils.selector(propensity.map( _._1 -> _._2 + add ).toMap.par)
  }

  def selectDemography(propensity: , add: , x_rand: ) = {}

  def selectCategory(recovery: Double, degradation: Double, fertility_loss: Double, management: Double, birth: Double, death: Double, x_rand: Double) = String {
    x_rand match {
      case $<recovery => "spontaneous"
      case $<recovery+degradation => "degradation"
      case $<recovery+degradation+fertility_loss => "fertility_loss"
      case $<recovery+degradation+fertility_loss+management => "management"
      case $<recovery+degradation+fertility_loss+management+birth => "birth"
      case $<recovery+degradation+fertility_loss+management+birth+death => "death"
    }
  }

}

object FragmentationUtils{

    // this would be useful for the updates in the fragments and all that
    def newNaturalUnit(unit: EcologicalUnit, fragments: ParSet[NaturalFragments], area: Int, esScalingExponent: Double) = ParSet[NaturalFragments]{
      val natural_neighbors = unit.manhattanNeighbors.filter( _.cover == "Natural" )

      natural_neighbors.size() match {
        case 0 => fragments.incl( NaturalFragment(Vector(unit)), esSupply(1, area, esScalingExponent) )
        // not sure what is the best syntax to include many fragments, this could lead to distinct fragments all containing  unit
        case _ => {
          val duplicated = fragments.collect( case f if f.contains( natural_neighbors.contains(_) ) => f )
          val new_merged_fragment = duplicated.flatten
        }
      }
    }

    def mergeFragments(fragment_1: NaturalFragment, fragment_2: NaturalFragment ){
      if fragment_1.share(fragment_2) => fragment_1.merge(fragment_2)
    }

    def cleanFragments(fragments: ParSet[NaturalFragments] ) {
      fragments.find( _.exists(_.contains(_)  ) )
      // create empty set to fill
      val new_fragments = ParSet[NaturalFragments]()
      fragments.subsets(2).foreach{ (f1, f2) =>
        if f1.share(f2) {
          // include the merge of the two fragments if they have a unit in common
          new_fragments.incl(f1.merge(f2))
        }
        else {
          new_fragments.incl(f1)
          new_fragments.incl(f2)
      }

    }

}
