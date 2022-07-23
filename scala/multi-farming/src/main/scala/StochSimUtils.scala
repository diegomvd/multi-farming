//
object StochSimUtils{
  def selectVId(x_rnd: Double,
                prob: ListMap[VertexId,Double]) = VertexId {
    prob.find(x_rnd <= _._2)._1
  }
}
