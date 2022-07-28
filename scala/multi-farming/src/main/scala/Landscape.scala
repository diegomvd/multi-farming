import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph

trait Landscape:

  val composition: Graph[A, Long]
  val size: Long

  def apply: Graph[A, Long]

  def updateComposition(g: Graph[A, Long], a: A, b: A): Graph[A, Long] = {
    g.mapValues(case a => b)
  }

  def updateComposition(g: Graph[A, Long], a: VertexRDD[A], b: A): Graph[A, Long] = {
    g.mapValues{ case (vid,attr) if a.contains((vid,attr)) => b }
  }
