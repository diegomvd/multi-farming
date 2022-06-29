/**
Here is defined the habitat network with the basic operations as add/remove
natural units and calculate connected components. Apache Graphx is used for the
implementation of graph objects and algorithms.
*/

import org.apache.spark._
import org.apache.spark.graphx._
// To make some of the examples work we will also need RDD
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph

case class HabitatNetwork(graph: Graph[ModuloCoord]){

  def connectedComponents : = {
    this.graph.connectedComponents
  }

}

object HabitatNetwork{

  /**
  * @param vertex is the natural unit to be removed
  * @param graph is the network describing habitat structure
  * @return graph without vertex nor the edges attached to graph
  */
  def habitatLoss(vertex: VertexId(ModuloCoord,ModuloCoord),
                  graph: Graph[ModuloCoord]): HabitatNetwork = {
    HabitatNetwork(graph.subgraph(vpred = _ == vertex))
  }

  def habitatGain(vertex: VertexId(ModuloCoord,ModuloCoord),
                  graph: Graph[ModuloCoord]): HabitatNetwork = {

  }
  /**
  * @param r is the radisu of the biophyisical landscape
  * @param ecr is the ecological connectivity range
  * @return a graph describing the habitat network in a pristine landscape
  */
  def buildGraph(r: Int, ecr: Int): Graph[ModuloCoord,ModuloCoord] = {
    val vertices = ModuloCoord.apply(r).toList
    // is the passage to rdd really needed ?
    val rdd_edges  = sc.parallelize( nodes.toSet.subsets(2).collect{
       case (c1,c2) if c1.manhattanNeighbors(r,ecr).exists(_ == c2) (c1,c2)
    }.toSeq )
    // I am not sure if the VertexId can be a ModuloCoord
    val edges: RDD[Edge[Int]] = rdd_edges.map{ (v1,v2) => Edge(v1,v2,1) }
    Graph.fromEdges(edges,defaultValue = 1)
  }

  /**
  * This function is used for initialization of habitat network in a pristine
  * landscape.
  *
  * @return a graph which is a spanning forest (SPF) of the natural network,
  * although here all the landscape is connected thus it is a spanning tree
  *
  *
  */
  def buildSPF(r: Int, ecr: Int) = Graph[ModuloCoord,UnDiEdge] {
    val vertices = ModuloCoord.apply(r).toList
    val edges = nodes.toSet.subsets(2).collect{
       case (coord1,coord2) if coord1.manhattanNeighbors(radius,threshold).exists(_ == coord2) UnDiEdge(coord1,coord2)
    }.toList
    Graph.from(nodes,edges)
  }

}








/**
All this is a draft for implementation of Raphtery. Might be of interest for
analysis of the results but probably not for the simulation itself.
*/
// class ModelSpout[(ModuloCoord,ModuloCoord)](edge: Vector[(ModuloCoord,ModuloCoord,Double)])
//   extends Spout[(ModuloCoord,ModuloCoord)]{
//
//     val e: (ModuloCoord,ModuloCoord) = this.edge
//
//     override def hasNext: Bool = {
//       e.hasNext
//     }
//
//     override def next(): (ModuloCoord, ModuloCoord) = {
//       e.next
//     }
// }
//
// class HabitatNetworkBuilder extends GraphBuilder[Vector[(ModuloCoord,ModuloCoord,Double)]]{
//
//   override def parseTuple(tuple: (ModuloCoord,ModuloCoord,Double)): Unit = {
//     sourceNode = tuple(0)
//     srcID      = assignID(sourceNode)
//     targetNode = tuple(1)
//     tarID      = assignID(tarNode)
//     timeStamp  = tuple(2)
//
//     addVertex(timeStamp, srcID, Properties(ImmutableProperty("Position",sourceNode)), Type("NatUnit"))
//     addVertex(timeStamp, tarID, Properties(ImmutableProperty("Position",sourceNode)), Type("NatUnit"))
//     addEdge(timeStamp, srcID, tarID, Type("Habitat Connection"))
//   }
// }
