/**
A planning unit is a set of EcoUnits that are managed together: conversion to
agriculture is thus made at the planning unit level. A PlnUnit is defined by
a VertexRDD with the VertexIds of the member EcoUnits as attribute. PlnUnits are
the elementary constituants of the planning landscape. Key functions are:
1- Finding ajacent EcoUnits to determine relative conversion weights
2- Determining availability: no EcoUnit belonging to the PlnUnit in agricultural state
*/

import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph
import scala.math.pow
import scala.math.max

case class PlnUnit(comp: ParVector[VertexId]){

  /**
  I don't know if adjacent makes sense anymore
  */
  def adjacent(r: Int): VertexRDD[VertexId] = {
    PlnUnit.adjacent(r,this.comp)
  }

  def isAvailable(eco: Graph[EcoUnit,Long]): Bool = {
    PlnUnit.isAvailable(this.comp,eco)
  }
}

object PlnUnit {

  /**
  @param r is the radius of the biophysical landscape
  @param comp the composition of the planning unit
  @return an RDD with the IDs of each ecological unit adjacent to the planning unit
  */
  def adjacent(r: Int,
               comp: ParVector[VertexId]): VertexRDD[VertexId] = {
    comp.mapValues( ModCo.neighbors(_.toInt,r,1) ).filterNot(comp.exists(_))
  }

  /**
  @param comp is the composition of the planning unit
  @param eco is the composition of the biophysical landscape
  @return true if the planning unit can be cultivated, false if not
  */
  def isAvailable(comp: ParVector[VertexId],
                  eco: Graph[EcoUnit,Long]): Bool = {
    comp.exists( eco.vertices.lookup(_).cover == "Natural") && comp.forall{ (eco.vertices.lookup(_).cover == "Natural" || eco.vertices.lookup(_).cover == "Degraded") }
  }

  /**
  @param nn is the number of neighbors that give weight to the clustering
  @param clst is the clustering coefficient
  @return the clustering weight
  */
  def weightExpression(nn: Int, clst: Double): Double = {
    pow( max(0.1,nn), clst)
  }

}
