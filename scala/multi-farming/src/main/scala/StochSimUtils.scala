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
