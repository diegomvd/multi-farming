import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

case class BioPhyLandscape(comp: ParMap[ModuloCoord, EcoUnit],
                           habnet: HabitatNetwork,
                           es: ParMap[ModuloCoord,Double]){
  /**
  * @param s is the sensitivity to ecosystem service provision
  * @param c is the land cover type to be matched
  * @param f is the function yielding a propensity given ecosystem service
  *          provision and sensitivity
  */
  def propensities(s: Double, c: String, f: (Double,Double) => Double) = ParMap[ModuloCoord,Double]{
    BioPhyLandscape.propensities(this.comp,this.es,s,c,f)
  }

  def updated(unit: EcoUnit): BioPhyLandscape =
    copy(this.radius, BioPhyLandscape.newComposition(unit,this.comp))

  /**
  Queries about the landscape's state
  **/
  def fractionNatural():       Double = this.comp.count{ _._2.isNatural() }
  def fractionDegraded():      Double = this.comp.count{ _._2.isDegraded() }
  def fractionLowIntensity():  Double = this.comp.count{ _._2.isLowIntensity() }
  def fractionHighIntensity(): Double = this.comp.count{ _._2.isHighIntensity() }

}

object BioPhyLandscape{

  /**
  * @param r is the radius of the biophysical landscape
  * @param t is the ecological connectivity range
  * @return a fresh biophysical landscape
  */
  def apply(r: Int,
            ecr: Int) = BioPhyLandscape {
    val comp = buildComposition(r,ecr)
    val habnet = buildHabitatNetwork()
    val es = buildESFlow()
    BioPhyLandscape(comp,habnet,es)
  }

  /**
  * @param r is the radius of the biophysical landscape
  * @param ecr is the ecological connectivity range
  * @return a biophysical composition with every unit in a natural state
  */
  def buildComposition(r: Int, ecr: Int) = ParMap[ModuloCoord, EcoUnit] {
    ModuloCoord.apply(r).map{ pos =>
      pos -> EcoUnit.apply(pos,pos.manhattanNeighbors(r,ecr),"Natural")
    }.toMap.par
  }

  /**
  * Initialization departs from a pristine landscape, thus all natural habitat
  * is contiguous and belongs to a unique, landscape size fragment
  * @param r is the radius of the biophysical landscape
  * @return habitat structure in the form of a single landscape size fragment
  */
  def buildHabitatNetwork(r: Int) = HabitatNetwork {
    ModuloCoord.apply(r).map // maybe needs to rely on the natural fragments factory
  }

  /**
  * Initialization departs from a pristine landscape, thus ecosystem services
  * flow is maximum and equal to 1 in every ecological unit in the landscape.
  * @param r is the radius of the biophysical landscape
  * @return the map of ecosystem service provision in a pristine landscape
  */
  def buildESFlow(r: Int) = ParMap[ModuloCoord,Double] {
    ModuloCoord.apply(r).map{ _ -> 1.0}
  }

  def updatedComposition(unit: EcoUnit, composition: ParMap[ModuloCoord, EcoUnit]) = ParMap[ModuloCoord, EcoUnit] {
    composition.map{ case (unit.coord(),_) => unit.coord() -> unit  }.toMap.par
  }

  def updatedHabitatNetwork() = NaturalFragments {}

  /**
  * @param loc is the positions where ES flow needs to be updated
  * @param comp is the biophysical composition of the landscape
  * @param habst is the biophysical habitat structure
  * @param z is the ES-area scaling exponent
  * @return a map with the updated values for ES flow in the needed units
  */
  def newESFlow(loc: Vector[ModuloCoord],
                comp: ParMap[ModuloCoord,EcoUnit],
                habst: HabitatStructure,
                z: Double) = ParMap[ModuloCoord,Double] {
     loc.map{c => c -> comp.get(c).esInFlow(comp,habst,z) }
  }

  /**
  * This function might be useful in very large landscapes or in a large
  * ecological connectivity range context, where we don't want to calculate ES
  * for every unit in the landscape. In small landscapes, or when landscape is
  * highly natural it might be faster to directly calculate for every unit
  * @param old is the current map of ecosystem service flow
  * @param upd is the new map of ES flow for the positions needing update
  * @return the updated map of ES flow
  */
  def updatedESFlow(old: ParMap[ModuloCoord,Double],
                    upd: ParMap[ModuloCoord,Double]) = ParMap[ModuloCoord] {
    old.map{ (c,old_es) => c -> upd.getOrElse(c, old_es) }
  }

  // this is a discrete time stochastic process to generate an initial landscape
  // faithful to the mechanisms of the model in the hope that the transient lenght
  // is diminished
  def apply() = {}

  def propensities(comp: ParMap[ModuloCoord, EcoUnit],
                   es: ParMap[ModuloCoord,Double],
                   s: Double,
                   c: String,
                   f: (Double,Double) => Double) = ParMap[ModuloCoord,Double] {
    comp.map{ (mc,eu) => mc -> EcoUnit.propensity(es.get(mc), s, eu.matchCover(c), f) }.toMap.par
  }


}
