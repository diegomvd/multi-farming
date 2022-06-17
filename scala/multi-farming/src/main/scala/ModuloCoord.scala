final case class ModuloCoord(m: Int){

  def toCubicHex(radius: Int) = PosCubicHex {
    // helper values
    val shift = 3 * radius + 2
    val ms = (this.m + radius) / shift
    val mcs = (this.m + 2) * radius / (shift - 1)

    // Need to make sure of this formula and check what is each coordinate
    // spatially
    val q = ms * (radius + 1) + mcs * radius
    val r = m + ms * (-2 * radius - 1) + mcs * (-radius - 1 )
    val s = -q - r

    PosCubicHex(q,r,s)
  }

  def manhattanNeighbors(radius: Int, threshold: Int) = List[ModuloCoord] {

    val posCubicHex = this.toCubicHex(radius)

    val qfrom = posCubicHex.q - threshold
    val qto = posCubicHex.q + threshold
    val rfrom = posCubicHex.r - threshold
    val rto = posCubicHex.r + threshold

    (qfrom to qto).flatMap(q => (rfrom to rto).map(PosCubicHex(q,_,-q-_).toModuloHex(radius))).toList
  }

}

object ModuloCoord{

  def apply(radius: Int)= Vector[ModuloCoord] {
   val area = 3 * radius * radius + 3 * radius + 1
   (0 until area).map(ModuloCoord(_)).toVector
  }

}
