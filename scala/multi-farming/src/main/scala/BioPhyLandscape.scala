import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

case class BioPhyLandscape(radius: Int, composition: ParMap[ModuloCoord, EcoUnit]){

  def recoveryPropensity() = ParMap[ModuloCoord, Double]{
    this.composition.map{
        case (mc,eu) if eu.isDegraded() => eu.esInFlow// put the function here
        case _ => mc -> 0.0
      }
    }.toMap.par
  }

  def degradationPropensity() = ParMap[ModuloCoord, Double]{
    this.composition.map{
        case (mc,eu) if eu.isNatural() => eu.esInFlow // put the function here
        case _ => mc -> 0.0
      }
    }.toMap.par
  }

  def fertilityLossPropensity() = ParMap[ModuloCoord, Double]{
    this.composition.map{
        case (mc,eu) if eu.isAgricultural() => eu.esInFlow // put the function here
        case _ => mc -> 0.0
      }
    }.toMap.par
  }

  def update(unit: EcoUnit): BioPhyLandscape =
    copy(this.radius, newComposition(unit,this.composition))

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
  This function builds the landscape's biosphysical composition and structure:
  a collection of EcoUnits with each EcoUnit storing its neighborhood
  **/
  def buildBioPhyLandscape(radius: Int, threshold: Int) = ParMap[ModuloCoord, EcoUnit] {
    ModuloCoord.apply(radius).map( pos => pos -> EcoUnit(pos,pos.manhattanNeighbors(radius,threshold),"Natural") ).toMap.par
  }

  /**
  Return the new compositionof a BioPhyLandscape after updating ECoUnit unit
  **/
  def newComposition(unit: EcoUnit, composition: ParMap[ModuloCoord, EcoUnit]) = ParMap[ModuloCoord, EcoUnit] {
    composition.map{ case (unit.coord(),_) => unit.coord() -> unit  }.toMap.par
  }
}









// def buildStructure(radius: Int, threshold: Int) = Graph[ModuloCoord,UnDiEdge] {
//   val nodes = ModuloCoord.apply(radius).toList
//   val edges = nodes.toSet.subsets(2).collect{
//      case (coord1,coord2) if coord1.manhattanNeighbors(radius,threshold).exists(_ == coord2) UnDiEdge(coord1,coord2)
//   }.toList
//   Graph.from(nodes,edges)
// }
