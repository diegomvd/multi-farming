import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph

trait Landscape:

  val composition: Graph[A, Long]
  val size: Int

  def apply: Graph[A, Long]

  def updateComposition(a: A, b: A): Graph[A, Long] =
    composition.mapValues(case a => b)

  def updateComposition(a: VertexRDD[A], b: A): Graph[A, Long] =
    composition.mapValues{ case (vid,attr) if a.contains((vid,attr)) => b }
