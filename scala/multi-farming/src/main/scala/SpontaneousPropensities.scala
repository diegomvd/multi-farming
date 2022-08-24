trait SpontaneousPropensities :

  val srec: Double
  val sdeg: Double
  val sflo: Double

  /**
  @param ival is the initial value for the cummulative sum
  @param es_graph is the biophysical composition of the landscape joined with the es flow
  @param s is this transition's sensitivity with es flow
  @param c is the land cover type required for this transition
  @param f is the function to calculate the propensity of this transition
  @return a vertexRDD with the propensity for a certain transition in each EcoUnit of the graph
  */
  def propensity(
    ival: Double,
    es_graph: Graph[(EcoUnit,Double),Long],
    s: Double,
    c: LandCover,
    f: (Double,Double) => Double): ListMap[VertexId,Double] = {
    val sg: Graph[(EcoUnit,Double),Long] = es_graph.subgraph(vpred = (_,(eu,_)) => eu.cover == c)
    val prop: ListMap[VertexId,Double] = ListMap(sg.vertices.mapValues{ (_,(_,es)) =>
       EcoUnit.propensity(es,s,f) }.collect.toSeq.sortWith(_._1 < _._1):_*)
    prop.scanLeft(-1L -> ival)( (pre, k -> v) => k -> v + pre._2 )
    // maybe it is better to scan left on the seq to tail there to then convert to a map
  }

  def recoveryPropensity(
    ival: Double,
    esgraph: Graph[(EcoUnit,Double),Long],
    s: Double):
    ListMap[VertexId,Double] =
      propensity(ival,esgraph,s,Degraded,EcoUnit.increasingPES)

  def degradationPropensity(
    ival: Double,
    esgraph: Graph[(EcoUnit,Double),Long],
    s: Double):
    ListMap[VertexId,Double] =
      propensity(ival,esgraph,s,Natural,EcoUnit.decreasingPES)

  def fertilityLossLIPropensity(
    ival: Double,
    esgraph: Graph[(EcoUnit,Double),Long],
    s: Double):
    ListMap[VertexId,Double] =
      propensity(ival,esgraph,s,LowIntensity,EcoUnit.decreasingPES)

  def fertilityLossHIPropensity(
    ival: Double,
    esgraph: Graph[(EcoUnit,Double),Long],
    s: Double):
    ListMap[VertexId,Double] =
      propensity(ival,esgraph,s,HighIntensity,EcoUnit.decreasingPES)

  /**
  @param ival is the initial value for the cummulative sum of the spontaneous propensities
  @param es_graph is the biophysical composition of the landscape joined with the es flow
  @param s is a tuple of the sensitivities for recovery,degradation and fertility loss transitions
  @return a tuple with the maps containing the propensities of each transition type and the last propensity value to continue cumulative sums
  */
  def spontaneousPropensities(
    ival: Double,
    es_graph: Graph[(EcoUnit,Double),Long]):
    ((ListMap[VertexId,Double],ListMap[VertexId,Double],ListMap[VertexId,Double],ListMap[VertexId,Double]), Double) = {
    val recovery: ListMap[VertexId,Double] = recoveryPropensity(ival,es_graph,srec)
    val degradation: ListMap[VertexId,Double] = degradationPropensity(recovery.last._2,es_graph,sdeg)
    val li_floss: ListMap[VertexId,Double] = fertilityLossLIPropensity(degradation.last._2,es_graph,sflo)
    val hi_floss: ListMap[VertexId,Double] = fertilityLossHIPropensity(li_floss.last._2,es_graph,sflo)
    ((recovery._1,degradation._1,li_floss._1,hi_floss._1),hi_floss.last._2)
  }
