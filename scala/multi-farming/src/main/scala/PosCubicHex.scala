final case class PosCubicHex(q: Int, r: Int, s: Int){

  // Returns the relative position vector between two positions, can be interesting
  // to eventually study spatial asymmetries
  def relPos(pos: PosCubicHex): VecCubicHex = VecCubicHex(PosCubicHex.q-q,PosCubicHex.r-r,PosCubicHex.s-s)

  // This function returns the modulo hexagonal coordinate to define periodic
  // border conditions
  def toModuloHex(radius: Int): PosModuloHex = {
    // helper values
    val area = 3 * radius * radius + 3 * radius + 1
    val shift = 3 * radius + 2
    val div = this.r + shift * this.q

    // "%" operator is the remainder operator in scala, with the function below we
    // obtain the modulo. Need to make sure this is right
    val m = ( (div % area) + area ) % area

    PosModuloHex(m)
  }

  // Returns all positions in the neighborhood of a position given the adjacency
  // threshold distance. Note that the distance treshold is an integer in this
  // case
  def manhattanNeighbors(distThreshold: Int, radius: Int): List[PosCubicHex] = {
    val qfrom = this.q-distThreshold
    val qto = this.q+distThreshold
    val rfrom = this.r-distThreshold
    val rto = this.r+distThreshold

    (qfrom to qto).flatMap(q => (rfrom to rto).map(PosCubicHex(q,_,-q-_).toModuloHex(radius).toCubicHex(radius))).toVector
  }

}

object PosCubicHex{}
