import scala.util.Random.nextInt

// @tparam EU is the exact case class that implements the ecological unit
case class Landscape(radius : Int,
                     connections : ParMap[PosModuloHex,Vector[PosModuloHex]],
                     composition : ParMap[PosModuloHex,EcologicalUnit]){

  // method to find a land unit given its position
  def findUnit(pos: PosModuloHex) : EcologicalUnit = map(getID(location, size)) // doesn't seem needed

  // method to update the landscape, unsure about the syntax...
  def update(pos: PosModuloHex, unit: EcologicalUnit): Landscape = new Landscape(this.radius, this.connections, this.composition.map{ case (pos,_) => pos -> unit })

}

object Landscape {

  // Builds the functional connections between the ecological units
  def buildConnections(radius: Int, threshold: Int): ParMap[PosModuloHex, Vector[PosModuloHex]] =
    PosModuloHex.apply(radius).map( pos => pos -> pos.manhattanNeighbors(radius,threshold).toVector ).toMap.par

  // Builds a pristine composition that is later updated in function of input parameters
  def buildPristineComposition(radius: Int): ParMap[PosModuloHex, EcologicalUnit] =
    PosModuloHex.apply(radius).map( pos => pos -> EcologicalUnit(pos,"Natural",1.0) ).toMap.par

  // This functions puts all the ecological units of a pristine landscape in a
  // single natural fragment. Will lead to errors if the composition map does
  // not correspond to a pristine landscape. /!\ Caution!
  def buildNaturalFragments(pristine_composition: ParMap[PosModuloHex, EcologicalUnit]): ParSet[NaturalFragment] =
    ParSet( NaturalFragment(pristine_composition.values().toVector) )

  def chooseStrategy(sparingFraction: Double) = String {
    sparingFraction >= rnd.nextDouble() match {
      case true => "Sparing"
      case false => "Sharing"
    }
  }

  def initStrategy(n_units: Int, sparingFraction: Double) = Map[Int, String] {
    (0 <- n_units).map{ _ -> chooseStrategy(sparingFraction) }.toMap
  }

  // to practical effects I only need to associate a position with a strategy. And I know for a fact that strategic units are voronoi tesselation
  // However for the future it can be nice to have an actual Strategic Units object
  def buildStrategicUnits(n_units: Int, radius: Int, sparingFraction: Double) = ParMap[PosModuloHex, String] {
    val strategies = initStrategy(n_units, sparingFraction)
    VoronoiUtils.voronoiTesselation(n_units, radius).map{ (pos,id) => pos -> strategies.get(id) }.toMap.par
  }

  def buildManagementUnits(radius: Int, management_scale: Double): // whatever voronoi inside strategic

  def buildESInflow(radius: Int): // whatever

  def apply(radius: Int ): Landscape = new Landscape(radius, buildConnections(radius,threshold), buildComposition(radius))

  def init(landscape: Landscape, degraded_units: Int, agricultural_units: Int) = Landscape {
    // Choose if next transition is a degradation or a conversion to agriculture
    if (agricultural_units > 0 & degraded_units > 0){
      if ( nextInt(degraded_units + agricultural_units) > agricultural_units ) {
        init( landscape.update( selectPos(landscape), EcologicalUnit(selectPos(landscape),"Degraded")), degraded_units-1, agricultural_units  )
      }
      else {
        init( landscape.updateMultiple( selectPos(landscape), ManageableUnit()  ), degraded_units, agricultural_units - manageable_unit.size() )
      }
    }
  }

}
