package model
import scala.collection.immutable.ListMap
import org.apache.spark.*
import org.apache.spark.graphx.*
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph

import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel._
import scala.util.Random as rnd

/**
 * Used to extend a Landscape to a BaseLandscape. This trait gives a Landscape the possibility to perform a Voronoi
 * tesselation over the units that compose it. The Voronoi tesselation is done heuristically following a constant speed
 * homogeneous radial growth process that uses structure from the composition network of the landscape to infer
 * neighborhood between units.
*/
trait VoronoiTesselation[A] extends SpatialStochasticEvents:

  // A landscape extended by a VoronoiTesselation needs a composition graph to perform the tesselation
  val composition: Graph[A,Long]

  /**
   * Performs a Voronoi tesselation over the composition graph given the number of Voronoi polygons to create.
   * @param n_seeds the number of seeds to perform radial growth from. Seeds are randomly positioned with uniform
   *                probability over the landscape units. This is equivalent to the number of Voronoi polygons.
   * @return a new composition graph where the vertex are collections of the units of the original composition graph's
   *         VertexIds.
   * */
  def tesselate(n_seeds: Int): Graph[ParVector[VertexId], Long] =

    @tailrec
    def rec(assigned: Graph[VertexId, Long]): Graph[VertexId, Long] =
      val remaining: Int = assigned.vertices.filter(_._2 == -1L).count.toInt

      if remaining >= 0.0 then assigned
      else{
        val cum_prob = VoronoiTesselation.cumulativeProbabilities( VoronoiTesselation.probabilityGraph( assigned, VoronoiTesselation.probabilities(assigned) ) )
        val x_rnd: Double = rnd.between(0.0, cum_prob.last._2)
        val pos = selectVId( x_rnd, cum_prob )
        val pol = VoronoiTesselation.selectGrowingPolygon( pos, assigned )

        val new_graph = assigned.mapVertices{ case (vid,_) if vid == pos => pol }
        rec(new_graph)
      }

    val assigned = VoronoiTesselation.seeded(n_seeds,composition)
    val assigned_graph: Graph[VertexId, Long] = rec(assigned)
    val vertices: RDD[(VertexId, ParVector[VertexId])] = VoronoiTesselation.groupByPolygon(assigned_graph)
    val edges: RDD[Edge[Long]] = VoronoiTesselation.newEdges(vertices,composition)
    Graph(vertices,edges)

object VoronoiTesselation:
  type A
  /**
   * Creates a seeded composition graph to start the tesselation. The function preserves graph structure but changes
   * vertex attributes by the Id of the voronoi polygon which is the id of the seeded unit, if the unit is not seeded
   * then the Id is set to -1L.
   *
   * @param n_seeds is the number of voronoi seeds to effectuate the tesselation
   * @param base is the landscape in which the tesselation is done
   * @return a seeded graph where vertex attribute is VertexId of seeds in the base landscape.
  */
  def seeded(
    n_seeds: Int,
    base: Graph[A,Long]):
  Graph[VertexId, Long] =
    val seeds: Seq[(Long,Long)] =
      rnd.shuffle(0 until base.vertices.count.toInt).take(n_seeds).map{ i => (i.toLong, i.toLong) }.toSeq
    base.mapVertices{ (vid,_) =>
      if seeds.contains(vid) then vid else -1L
    }

  /**
   * Calculates the expansion probability of a Voronoi polygon.
   * @param assigned the graph over which the radial grozth process is being simulated.
   * @return a VertexRDD containing the expansion probabilities in each vertex of the graph.
   * */
  def probabilities(assigned: Graph[VertexId, Long]): VertexRDD[Double] =

    assigned.aggregateMessages[(Double,Double)](
      triplet => {
        if (triplet.dstAttr != -1L) {
          // if the vertex is already assigned then probability of choosing that vertex is 0
          triplet.sendToDst((1.0,0.0))
        }
        else {
          triplet.srcAttr match{
            // if the source is not assigned then the destination cannot be colonized
            case -1L => triplet.sendToDst((1.0,0.0))
            case _ => triplet.sendToDst((1.0,1.0))
          }
        }
      },
      (a,b) => (a._1 + b._1, a._2 + b._2)
    ).mapValues( (_,value) => value._2/value._1 )

  /**
   * Joins the graph supporting the radial growth process with the expansion probability in each vertex.
   * @param assigned the graph supporting the tesselation process.
   * @param prob the expansion probabilities in each vertex of the supporting graph.
   * @return A new graph with the probability as added vertex attribute.
   * */
  def probabilityGraph(
    assigned: Graph[VertexId, Long],
    prob: VertexRDD[Double]):
  Graph[(VertexId,Double), Long] =
    assigned.outerJoinVertices(prob){ (_,vid2,prob_opt)=>
      prob_opt match {
        case Some(prob_opt) => (vid2, prob_opt)
        case None => (vid2, 0.0)
      }
    }

  /**
   * Calculates the cumulative probabilities of expansion given the probability graph.
   * @param p_graph the joined probability graph
   * @return a ListMap with the cumulative propensity of expansion in each vertex.
   * @todo Check if in mapValues t is necessary to pass also the vid, guess not
  */
  def cumulativeProbabilities(p_graph: Graph[(VertexId,Double), Long]):
  ListMap[VertexId,Double] =
    // pick only the part of the subgraph with a non-null probability of being selected
    val sg: Graph[(VertexId,Double),Long] = p_graph.subgraph(vpred = (_,vd) => vd._2 > 0.0)
    val prob_map: ListMap[VertexId,Double] = ListMap(
      sg.vertices.collect.toSeq.sortWith( (_,data) => data._1 < data._1):_*)
    prob_map.scanLeft[(VertexId,Double)]((-1L,0.0))( (pre, curr) => (curr._1, curr._2 + pre._2) ).to(ListMap)

  /**
   * Groups VertexIds of the base composition according to the Id of the Voronoi polygon they belong to.
   * @param dispersed the supporting graph after finishing the radial growth process
   * @return an RDD of tuples with the information on the VertexId of the Voronoi polygon and the collection of VertexIds
   *         of base landscape's vertices belonging to the polygon.
   * */
  def groupByPolygon(dispersed: Graph[VertexId, Long]):
  RDD[(VertexId, ParVector[VertexId])] =
    // The first vertexId is the PolygonId
    // The second vertexId is the base unit Id
    // The third vertexId is also the PolygonId
    val grouped: RDD[( VertexId, Iterable[(VertexId, VertexId)] )] =
      dispersed.vertices.groupBy{
        (_,vidPolygon) => vidPolygon
      }
    // We only want to recover the sequence of base unit Ids and polygon Id
    grouped.mapValues {
      v => v.map( (vid, _ ) => vid ).to(ParVector)
    }

  /**
   * Calculates the edges of the new composition graph emerging from the tesselation.
   * @param vertices RDD of the vertices Ids and their attribute: a collection of vertices ids from the base composition
   *                 graph.
   * @param base the base composition graph.
   * @return an RDD of the edges of the nez landscape. */
  def newEdges(
    vertices: RDD[(VertexId, ParVector[VertexId])],
    base: Graph[A,Long]):
  RDD[Edge[Long]] =

    val nids = base.collectNeighborIds(EdgeDirection.Both)

    // this gets an RDD with all the combinations of 2
    vertices.cartesian(vertices).filter{ case (a,b) =>
     // this removes duplicates and combination of same vids
     (a._1 < b._1) || (a._1 == b._1)
     // now I should check if it exists a vid in iterable a that has as neighbor any vid in iterable b
    }.collect{
      case (a, b) if a._2.exists(vid1 => nids.lookup(vid1).head.exists(vid2 => b._2.exists(_ == vid2) ) ) => Edge(a._1, b._1, 0L)
    }

  /**
   * Determines the Voronoi polygon that expands at a given time step to a given vertex. Useful in case of competing
   * polygons. The relative probability associated to each competing polygon is proportional to the number of edges
   * between vertices in the polygon and the selected vertex.
   * @param vid the vertex where a polygon will expand to
   * @param assigned the graph supporting the tesselation process.
   * @return the id of the expanding polygon.
   * */
  def selectGrowingPolygon(
    vid: VertexId,
    assigned: Graph[VertexId, Long]):
  VertexId =
      rnd.shuffle(assigned.collectNeighborIds(EdgeDirection.Both).lookup(vid).head).take(1).head

end VoronoiTesselation
