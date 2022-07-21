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

  @tailrec
  def voronoiRadialGrowth(radius: Int, cells: ParMap[ModuloCoord, Int]) = ParMap[ModuloCoord, Int]{
    val area = 3 * radius * radius + 3 * radius + 1
    if (cells.size() >= area) cells
    else{
      val pos = S3Utils.positionSelector( voronoiProbability(radius, cells) )
      val cell = S3Utils.eventSelector( radius, pos, cells )
      val new_cells = cells.map( case (_,id) => pos -> id ).toMap.par
      voronoiRadialGrowth( radius, pos, new_cells)
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


  def selectPosition(propensity: ParMap[ModuloCoord,Double], add: Double, x_rand: Double) = ModuloCoord {
    S3Utils.positionSelector(propensity.map( _._1 -> _._2 + add ).toMap.par)
  }


}
