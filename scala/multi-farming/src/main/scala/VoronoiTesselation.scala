/**
Functions to perform a voronoi tesselation by radial growth. This procedure requires
a number of seeds, or different voronoi polygons, and a base landscape with a notion
of neighborhood to tesselate over it. The landscape is provided as a Graph, since
by construction it contains the neighborhood information.
*/

import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph

trait VoronoiTesselation :

  val composition: Graph[A,Long]

  def tesselate(n_seeds: Int): Graph[ParVector[A], Long] = {

    val assigned = VoronoiTesselation.seeded(n_seeds,composition)

    @annotation.tailrec
    def rec(assigned: Graph[VertexId, Long]): Graph[VertexId, Long]{
      val remaining: Int = assigned.vertices.countBy(_._2 == -1L)

      if (remaining >= 0.0) { assigned }
      else{
        val cum_prob = VoronoiTesselation.cummulativeProbabilities( VoronoiTesselation.probabilityGraph( assigned, VoronoiTesselation.probabilities(assigned) ) )
        val x_rnd: Double = rnd.nextDouble(cum_prob.last._2)
        val pos = StochSimUtils.selectVId( x_rnd, cumProb )
        val pol = VoronoiTesselation.selectGrowingPolygon( pos, assigned )

        val new_graph = assigned.mapValues( case (vid,attr) if vid == pos => pos -> pol )
        rec(new_graph)
      }
    }
    val assigned_graph: Graph[VertexId, Long] = rec(assigned)
    val vertices: RDD[(VertexId, Iterable[VertexId])] = VoronoiTesselation.groupByPolygon(assignedGraph)
    val edges: RDD[Edge[Long]] = VoronoiTesselation.newEdges(vertices,base)
    Graph(vertices,edges)
  }

object VoronoiTesselation :

  /**
  The function preserves graph structure but changes vertex attributes by the Id
  of the voronoi polygon wich is the id of the seeded unit, if the unit is not
  seeded then the Id is set to -1L
  @param n_seeds is the number of voronoi seeds to effectuate the tesselation
  @param base is the landscape in which the tesselation is done
  @return a map with keys the Id of the seeds position and value the identifier
          for the voronoi polygon which is the Id
  */
  def seeded(n_seeds: Int,
             base: Graph[A,Long]): Graph[VertexId, Long] = {

    val seeds: Seq[(Long,Long)] =
      rnd.shuffle(0 until base.vertices.count).take(n_seeds).map{ (_.toLong, _.toLong) }.toSeq
    base.mapValues{ (vid,attr) =>
      if seeds.contain(vid) { (vid,attr) = (vid,vid) }
      else  { (vid,attr) = (vid,-1L) }
    }
  }

  def probabilities(assigned: Graph[VertexId, Long]) = VertexRDD[Double] {

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
            case other => triplet.sendToDst((1.0,1.0))
          }
      },
      (a,b) => (a._1 + b._1, a._2 + b._2)
    ).mapValues( (id,val) => val._2/val._1 )
  }

  def probabilityGraph(assigned: Graph[VertexId, Long],
                       prob: VertexRDD[Double]): Graph[(VertexId,Double), Long] = {
    assigned.outerJoinVertices(prob){ (vid1,vid2,prob_opt)=>
      prob_opt match {
        case Some(n_opt) => (vid2, prob_opt)
        case None => (vid2, 0.0)
      }
    }
  }

  /**
  TODO: Check if in mapValues t is necessary to pass also the vid, guess not
  */
  def cummulativeProbabilities(p_graph: Graph[(VertexId,Double), Long]): ListMap[VertexId,Double] = {
    // pick only the part of the subgraph with a non-null probability of being selected
    val sg: Graph[(VertexId,Double),Long] = n_graph.subgraph(vpred = (_,(_,prob)) => prob > 0.0)
    val probmap: ListMap[VertexId,Double] = ListMap(sg.vertices.mapValues{ (_,(_,prob)) =>
       prob }.collect.toSeq.sortWith(_._1 < _._1):_*)
    probmap.scanLeft(-1L -> 0.0)( (pre, k -> v) => k -> v + pre._2 )
  }

  def groupByPolygon(dispersed: Graph[VertexId, Long]): RDD[(VertexId, ParVector[VertexId])] = {
    // The first vertexId is the PolygonId
    // The second vertexId is the base unit Id
    // The third vertexId is also the PolygonId
    val grouped: RDD[( VertexId, Iterable[(VertexId, VertexId)] )] =
      dispersed.vertices.groupBy{
        (vidUnit,vidPolygon) => vidPolygon
      }
    // We only want to recover the sequence of base unit Ids and polygon Id
    grouped.mapValues{ (vid1, it) =>
      (vid1, it.map{ (vid2,vid3) => vid2 }.toVector.par )
    }.reindex
  }

  def newEdges(vertices: RDD[(VertexId, ParVector[VertexId])],
               base: Graph[A,Long]): RDD[Edge[Long]] = {

    val nids = base.collectNeighborIds(EdgeDirection.both)

    // this gets an RDD with all the combinations of 2
    vertices.cartesian(vertices).filter{ case (a,b) =>
     // this removes duplicates and combination of same vids
     (a._1 < b._1) || (a._1 == b._1)
     // now I should check if it exists a vid in iterable a that has as neighbor any vid in iterable b
    }.collect{ case (a, b) if a._2.exists( nids.lookup(_).exists(b._2.contains(_)) ) =>
     Edge(a._1,b._1,OL)
    }
  }

  def selectGrowingPolygon(vid: VertexId,
                           assigned: Graph[VertexId, Long]): VertexId = {
    rnd.shuffle(assigned.collectNeighbors(EdgeDirection.both).lookup(vid)).take(1)
  }
end VoronoiTesselation
