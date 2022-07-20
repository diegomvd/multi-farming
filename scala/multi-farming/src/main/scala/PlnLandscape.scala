/**
The PlnLandscape object provides all the functions needed to interact with a
planning landscape represented by a graph of PlnUnits. Key functions are:
1- Build a planning landscape given the radius of the biophysical landscape
2- Determine neighbor availability to influence conversion weights
3- Get all the planning units within a management unit extended by their neighborhood
*/

import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph
import scala.math.pow
import scala.math.max

object PlnLandscape{

  /**
  * This function is a preliminary computation of the planning landscape's
  * composition.
  *  TODO: abstract voronoiTesselation
  *  @param nu is the number of planning units to create
  *  @param nt is the total number of ecological units in the biophysical landscape
  *  @return a map storing the id and composition of each planning unit
  */
  def buildCompMap(nu: Int, nt: Int): ParMap[Long, PlnUnit] = {
    VoronoiUtils.voronoiTesselation(nu,nt).groupBy( _._2 ).map{ (key, val) =>
      key.toLong -> val.values.toSet
    }.toMap.par
  }

  /**
  *  @param nu is the number of planning units to create
  *  @param r is the radius of the biophysical landscape
  *  @return the composition graph of the planning landscape
  */
  def build(nu: Int, r: Int): Graph[PlnUnit, Long] = {

    val nt = 3 * r * r + 3 * r + 1
    val precomp = prepareComposition(nu,nt)

    val sc: SparkContext
    // the units are defined by a vertex id which is the id of the PlnUnit
    // and a VertexRDD of VertexIds from to the eco composition graph
    // and representing the EcoUnits belongin to the PlnUnit.
    val units: RDD[(VertexId, PlnUnit)] =
      sc.parallelize( precomp.map{ (_._1,_._2) }.toSeq )

    // An edge between 2 PUs exists if PU1 has an adjacent EcoUnit that belongs
    // to PU2
    val edges: RDD[Edge[Long]] =
      sc.parallelize( precomp.toSet.subsets(2).collect{ // using subsets guarantees no repeated operation
        case (pu1,pu2) if pu1._2.adjacent(r).exists(_ == pu2._2) =>
          Edge(pu1._1,pu2._1,0L)
        }
      )
    Graph(units,edges)
  }

  /**
  @param comp is the composition graph of the selected management unit
  @param eco is the composition of the biophysical landscape
  @return the number of available neighbors for each available unit
  */
  def availableNeighbors(comp: Graph[PlnUnit,Long],
                         eco: Graph[EcoUnit,Long]): VertexRDD[Int] = {
    comp.aggregateMessages[Int](
      triplet => {
        if (triplet.dstAttr.isAvailable(eco)){
          if (triplet.srcAttr.isAvailable(eco)) {
            triplet.sendToDst(1)
          }
          else triplet.sendToDst(0)
        }
      },
      (a,b) => a + b
    )
  }

  /**
  @param comp is the composition graph of the selected management unit
  @param eco is the composition of the biophysical landscape
  @return the number of unavailable neighbors for each available unit
  */
  def unavailableNeighbors(comp: Graph[PlnUnit,Long],
                           eco: Graph[EcoUnit,Long]): VertexRDD[Int] = {
    comp.aggregateMessages[Int](
      triplet => {
        if (triplet.dstAttr.isAvailable(eco)) {
          if (triplet.srcAttr.isAvailable(eco)) {
            triplet.sendToDst(0)
          }
          else triplet.sendToDst(1)
        }
      },
      (a,b) => a + b
    )
  }

  /**
  * returns the subgraph of the planning landscape belonging to a management unit
  * with the adjacent pus, at the moment is not extended
  */
  def extendedSubGraph(comp: Graph[PlnUnit,Long],
                       sub: VertexRDD[VertexId]): Graph[PlnUnit,Long] = {
    comp.subgraph( vpred = (vid,attr) => sub.contains(vid) )
  }

}
