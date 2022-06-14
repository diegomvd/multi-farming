case class ManagementLandscape( units: ParMap[Int, StrategicUnit] ){

  def voronoiColonizationProb() = ManagementLandscape {

  }

  def belongs(pos: PosModuloHex){
    this.units.foreach{ (id,unit) => if (unit.composition.exists( (_,p) => p == pos )) true }
  }

}

object ManagementLandscape{}
