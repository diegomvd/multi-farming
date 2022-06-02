import scala.util.Random

final case class PosCubicHex(q: Int, r: Int, s: Int){

  // Returns the relative position vector between two positions, can be interesting
  // to eventually study spatial asymmetries
  def relPos(pos: PosCubicHex): VecCubicHex = VecCubicHex(PosCubicHex.q-q,PosCubicHex.r-r,PosCubicHex.s-s)

}

object PosCubicHex{
   // Returns all positions for a regular hexagonal lattice of edge length size

   // Returns all positions in the neighborhood of a position given the adjacency
   // distance dist
   def neighbors(pos: PosCubicHex, dist: Double): Vector[PosCubicHex] = {

   }
}
