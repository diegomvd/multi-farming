/**
Functions to perform a voronoi tesselation by radial growth. This procedure requires
a number of seeds, or different voronoi polygons, and a base landscape with a notion
of neighborhood to tesselate over it. The landscape is provided as a Graph, since
by construction it contains the neighborhood information.
*/
object VoronoiTesselation{

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

  @tailrec
  def tesselation(n_seeds: Int,
                  base: Graph[A,Long]) = Graph[VertexId, Long]{

    val assigned = seeded(n_seeds,base)
    val cum_prob = cummulativeProbabilities( probabilityGraph( assigned, probabilities(assigned) ) )


    def rec(assigned: Graph[VertexId, Long]): Graph[VertexId, Long]{
      val remaining: Int = assigned.vertices.countBy(_._2 == -1L)

      if (remaining >= 0.0) { assigned }
      else{
        val cum_prob = cummulativeProbabilities( probabilityGraph( assigned, probabilities(assigned) ) )
        val pos = S3Utils.positionSelector( cumProb )
        val pol = S3Utils.polygonSelector( pos, assigned )

        val new_graph = assigned.mapValues( case (vid,attr) if vid == pos => pos -> pol )
        rec( new_graph)
      }
    }

    if (remaining >= 0.0) { assigned }
    else{
      val seeds = seeded(n_seeds,base)


      val pos = S3Utils.positionSelector( cumProb )
      val pol = S3Utils.polygonSelector( radius, pos, cells )
      val new_cells = cells.map( case (_,id) => pos -> id ).toMap.par
      voronoiRadialGrowth( radius, pos, new_cells)
    }
  }

  def voronoiTesselation(n_seeds: Int, radius: Int) = ParMap[ModuloCoord,Int]{
    VoronoiUtils.voronoiRadialGrowth(radius, VoronoiUtils.seedVoronoiTesselation(n_seeds,radius))
  }

object S3Utils{ // utility functions for spatial stochastic simulations

  // could be transformed to ParMap[Int,Double] to make it more general
  def positionSelector(prob: ParMap[ModuloCoord, Double]) = ModuloCoord {
    // one liner to get the position by ordering the map into a vector map
    val x_rand = rnd.nextDouble(1.0)
    VectorMap(prob.toSeq.sortBy(_._1.m):_*).scanLeft[(ModuloCoord,Double)]((ModuloCoord(-1),0.0)){case ((_,acc),(pos,x)) => (pos, acc + x)}.find((_,x) => x_rand <= x)._1
  }


  def selectPosition(propensity: ParMap[ModuloCoord,Double], add: Double, x_rand: Double) = ModuloCoord {
    S3Utils.positionSelector(propensity.map( _._1 -> _._2 + add ).toMap.par)
  }


}
