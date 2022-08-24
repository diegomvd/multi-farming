/**
A Planning Unit is a set of EcoUnits that are managed together: conversion to
agriculture is made at the Planning Unit level. Planning Units are implemented
in the PlnUnit case class and companion object. A PlnUnit is described by its
composition which is a parallelized vector containing the VertexIds of the
EcoUnits within the EcoLandscape.
Planning Units are the elementary constituants of the Planning Landscape.
At the moment Planning Units are set at initialization by performing a Voronoi
tesselation in its base landscape which is the EcoLandscape. Its composition
cannot be updated for now.
Key functions are:
1- Finding ajacent EcoUnits to determine relative conversion weights
2- Determining availability: no EcoUnit belonging to the PlnUnit in agricultural state

@author diego

TODO: determine whether the adjacent function still makes sense.
*/

import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.Edge
import org.apache.spark.graphx.Graph
import scala.math.pow
import scala.math.max

case class PlnUnit(composition: ParVector[VertexId]):
  /**
  I don't know if adjacent makes sense anymore
  */
  def adjacent(r: Int): ParVector[VertexId] =
    PlnUnit.adjacent(r,this.composition)

  def isAvailable(eco: Graph[EcoUnit,Long]): Bool =
    PlnUnit.isAvailable(this.composition,eco)

object PlnUnit :

  /**
  @param r is the radius of the biophysical landscape
  @param comp the composition of the planning unit
  @return an RDD with the IDs of each ecological unit adjacent to the planning unit
  */
  def adjacent(
    r: Int,
    comp: ParVector[VertexId]):
    ParVector[VertexId] =
      comp.mapValues( ModCo.neighbors(_.toInt,r,1) ).filterNot(comp.exists(_))

  /**
  @param comp is the composition of the planning unit
  @param eco is the composition of the biophysical landscape
  @return true if the planning unit can be cultivated, false if not
  */
  def isAvailable(
    comp: ParVector[VertexId],
    eco: Graph[EcoUnit,Long]):
    Bool =
      val predicate1 = comp.exists( eco.vertices.lookup(_).matchCover(Natural))
      val predicate2 = comp.forall{ (eco.vertices.lookup(_).matchCover(Natural) || eco.vertices.lookup(_).matchCover(Degraded)) }
      predicate1 && predicate2

  /**
  @param nn is the number of neighbors that give weight to the clustering
  @param clst is the clustering coefficient
  @return the clustering weight
  */
  def weightExpression(
    nn: Int,
    clst: Double):
    Double =
      pow( max(0.1,nn), clst)

end PlnUnit
