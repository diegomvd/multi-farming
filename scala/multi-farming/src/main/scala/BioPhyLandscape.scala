import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

case class BioPhyLandscape(composition: ParMap[ModuloCoord, EcoUnit],
                           habitat_structure: NaturalFragments,
                           es_flow: ParMap[ModuloCoord,EcoUnit]){
  /**
  * @param s is the sensitivity to ecosystem service provision
  * @param c is the land cover type to be matched
  * @param f is the function yielding a propensity given ecosystem service
  *          provision and sensitivity
  */
  def propensities(s: Double, c: String, f: (Double,Double) => Double) = ParMap[ModuloCoord,Double]{
    BioPhyLandscape.propensities(this.composition,this.es_flow,s,c,f)
  }

  def update(unit: EcoUnit): BioPhyLandscape =
    copy(this.radius, BioPhyLandscape.newComposition(unit,this.composition))

  /**
  Queries about the landscape's state
  **/
  def radius(): Int = this.radius
  def fractionNatural():       Double = this.composition.count{ _._2.isNatural() }
  def fractionDegraded():      Double = this.composition.count{ _._2.isDegraded() }
  def fractionLowIntensity():  Double = this.composition.count{ _._2.isLowIntensity() }
  def fractionHighIntensity(): Double = this.composition.count{ _._2.isHighIntensity() }

}

object BioPhyLandscape{

  /**
  * @param r is the radius of the biophysical landscape
  * @param t is the ecological connectivity range
  * @return a fresh biophysical landscape
  */
  def apply(r: Int,
            cr: Int) = BioPhyLandscape {
    val comp = buildComposition(r,cr)
    val es = buildESFlow()
    val habst = buildHabitatStructure()
    BioPhyLandscape(comp,es,habst)
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

  // need create habitat and es flow to be able to initialize landscape
  def buildHabitatStructure() = NaturalFragments {}
  def buildESFlow() = ParMap[ModuloCoord,Double] {}

  def updateComposition(unit: EcoUnit, composition: ParMap[ModuloCoord, EcoUnit]) = ParMap[ModuloCoord, EcoUnit] {
    composition.map{ case (unit.coord(),_) => unit.coord() -> unit  }.toMap.par
  }

  def updateHabitatStructure() = NaturalFragments {}

  def updateESFlow() = ParMap[ModuloCoord,Double] {}

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









// def buildStructure(radius: Int, threshold: Int) = Graph[ModuloCoord,UnDiEdge] {
//   val nodes = ModuloCoord.apply(radius).toList
//   val edges = nodes.toSet.subsets(2).collect{
//      case (coord1,coord2) if coord1.manhattanNeighbors(radius,threshold).exists(_ == coord2) UnDiEdge(coord1,coord2)
//   }.toList
//   Graph.from(nodes,edges)
// }
