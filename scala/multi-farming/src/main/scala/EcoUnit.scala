case class EcoUnit(pos: ModuloCoord, neighbors: Vector[ModuloCoord], cover: String){

  def pos: Double = this.pos
  def neighbors: Vector[ModuloCoord] = this.neighbors
  def cover: String = this.cover

  def matchCover(c: String) = Bool { EcoUnit.matchCover(this.cover,c) }

  def updateCover(cover: String) = Option[EcoUnit] { copy(this.pos, this.neighbors, cover) }

  def esInFlow(comp: ParMap[ModuloCoord,EcoUnit],
               habst: HabitatStructure, // maybe just need to pass composition
               z: Double) = Double {
      EcoUnit.esInFlow( this.naturalNeighbors(comp) , this.neighbors.size, habst, z)
  }

  def naturalNeighbors(comp: ParMap[ModuloCoord,EcoUnit]) = Vector[ModuloCoord]{
    EcoUnit.naturalNeighbors(this.neighbors,comp)
  }

}

object EcoUnit{

  /**
  * @param c1 first land cover type
  * @param c2 second land cover type
  * @return true if both covers are equal, false if not
  */
  def matchCover(c1: String, c2: String) = Bool { c1 == c2 }

  /**
  * @param s is the sensitivity to ecosystem service inflow
  * @param es is the ecosystem service inflow
  * @param bool determines whether the unit has the right cover
  * @return the recovery/degradation propensity
  */
  def recoveryEquation(s: Double, es: Double, bool: Bool) = Double {
    if bool s*es
    else 0.0
  }
  def degradationEquation(s: Double, es: Double, bool: Bool) = Double {
    if bool (1-es)*s
    else 0.0
  }
  /**
  * @param f is either recovery/degradationEquation
  * @return the propensity given the chosen function
  */
  def propensity(s: Double,
                 es: Double,
                 bool: Bool,
                 f: (Double,Double,Bool) => Double) = Double {
    f(es,s,bool)
  }

  /**
  * The function iterates over the natural neighbors of the unit, finds their
  * natural fragment and computes its contribution to ES provision
  * @param neigh are the coords of the ecological unit's natural neighbors
  * @param n_size is the total neighborhood size
  * @param habst is the biophysical landscape's habitat structure
  * @param z is the ecosystem service provision-area scaling exponent
  * @return the ecosystem service inflow in this ecological unit
  */
  def esInFlow(neigh: Vector[ModuloCoord],
               n_size: Double,
               habst: HabitatStructure, // maybe just need to pass composition
               z: Double) = Double {
    neigh.{ n => habst.find( _.contains(n)).es(z) }.reduce(_+_).div(n_size).toDouble
  }

  /**
  * @param neigh is the neighborhood of this unit
  * @param comp is the biophysical composition of the landscape
  * @return the natural neighbors of this unit
  */
  def naturalNeighbors(neigh: Vector[ModuloCoord],
                       comp: ParMap[ModuloCoord,EcoUnit]) = Vector[ModuloCoord] {
    neigh.filter{ comp.get(_).cover == "Natural" }
  }
}
