import scala.math.pow
import scala.math.max

case class PlanningUnit(id: Int,
                        comp: Set[ModuloCoord],
                        eco_neighbors: Vector[ModuloCoord],
                        neighbors: Vector[Int]){

  def id = this.id
  def composition = this.comp
  def eco_neighbors = this.eco_neighbors
  def neighbors = this.neighbors

  def isAvailable(biocomp: ParMap[ModuloCoord,EcoUnit]): Bool =
    PlanningUnit.isAvailable(this.comp, biocomp)

  def conversionWeight(biocomp: ParMap[ModuloCoord,EcoUnit],
                       strategy: String) = Double {
    PlanningUnit.conversionWeight(this.comp,this.eco_neighbors,biocomp,strategy)
  }

}

object PlanningUnit {

  /**
  * @param radius is the radius of the biophysical landscape
  * @param comp the composition of the planning unit
  * @return a vector with the coordinates of each ecological unit adjacent to the planning unit
  */
  def ecoNeighbors(radius: Int,
                   comp: Set[ModuloCoord]) = Vector[ModuloCoord]{
    comp.map( _.manhattanNeighbors(radius,1) ).toVector
  }

  /**
  * @param comp is the composition of the planning unit
  * @param biocomp is the composition of the biophysical landscape
  * @return true if the planning unit can be cultivated, false if not
  */
  def isAvailable(comp: Set[ModuloCoord],
                  biocomp: ParMap[ModuloCoord,EcoUnit]) = Bool{
    comp.exists(biocomp.get(_).cover == "Natural") && comp.forall{ (biocomp.get(_).cover == "Natural" || biocomp.get(_).cover == "Degraded") }
  }

  /**
  * @param nn is the number of neighbors that give wieht to the clustering
  * @param clst is the clustering coefficient
  * @return the clustering weight
  */
  def weightExpression(nn: Int, clst: Double) = Double{
    pow( max(0.1,nn), clst)
  }

  /**
  * @param comp is the composition of the planning unit
  * @param econ is the ecological neighbors of this planning unit
  * @param biocomp is the composition of the biophysical landscape
  * @param strategy is the land use management strategy in the management unit containing this planning unit
  * @return the clustering weight for this planning unit
  */
  def conversionWeight(comp: Set[ModuloCoord],
                       econ: Vector[ModuloCoord],
                       biocomp: ParMap[ModuloCoord,EcoUnit],
                       strategy: String) = Double {
    if isAvailable(comp,biocomp) {
      /**
      * TODO: refactor the neighborhood size and weight equation and create a
      * land-use strategy class that contains the clustering coeff
      */
      strategy match {
        case "Sparing" => {
          val nn = econ.filterNot{biocomp.get(_).cover == "Natural"}.toVector.size
        }
        case "Sharing" => {
          val nn = econ.filter{biocomp.get(_).cover == "Natural"}.toVector.size
        }
      weightExpression(nn,clst)
      }
    }
    else 0.0
  }

}
