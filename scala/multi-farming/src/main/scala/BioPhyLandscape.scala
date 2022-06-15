import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

case class Landscape(biophysical: ParMap[ModuloCoord, EcoUnit], management:  )


case class BioPhyLandscape(composition : ParVector[EcoUnit], structure: Graph[ModuloCoord,UnDiEdge]){

  def neighbors(unit: EcoUnit) = Vector[EcoUnit] {

  }

}

object BioPhyLandscape{

  def buildComposition(radius: Int) = ParVector[EcoUnit] {
    ModuloCoord.apply(radius).map( pos => EcoUnit(pos,"Natural") ).toVector.par
  }

  def buildStructure(radius: Int, threshold: Int) = Graph[ModuloCoord,UnDiEdge] {
    val nodes = ModuloCoord.apply(radius).toList
    val edges = nodes.toSet.subsets(2).collect{
       case (coord1,coord2) if coord1.manhattanNeighbors(radius,threshold).exists(_ == coord2) UnDiEdge(coord1,coord2)
    }.toList
    Graph.from(nodes,edges)
  }

}

case class ManagementLandscape(composition: ParVector[ManagementUnit], structure: ParSet[ManagementRegion]){

}

object ManagementLandscape{

  def buildComposition(n_units: Int, radius: Int) = ParVector[ManagementUnit] {
    /**
    After performing a Voronoi tesselation over EcoUnits positions, units
    are grouped by voronoi cell id and and a ManagementUnit is initialized per
    set of EcoUnits in the same voronoi cell.
    Management unit composition should be a set of ecounits to facilitate look up
    **/
    val area = 3 * radius * radius + 3 * radius + 1
    VoronoiUtils.voronoiTesselation(n_units,area).groupBy( _._2 ).map{
      (key, val) => ManagementUnit(val.values.toSet, "None")
    }.toVector.par
  }

  def buildStructure(n_regions: Int, n_units: Int) = ParSet[ManagementRegion] {
    // beware, I need to generalize the voronoiUtils object so that not only positions
    // are returned from the tesselation
    VoronoiUtils.voronoiTesselation(n_regions,n_units).groupBy( _._2 ).map{
      (key, val) => ManagementRegion(val.values.toSet, "None")
    }.toSet.par
  }

}
