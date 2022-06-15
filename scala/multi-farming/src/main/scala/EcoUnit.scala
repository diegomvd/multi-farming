case class EcolUnit(pos: ModuloCoord, neighbors: Vector[ModuloCoord], cover: String = "Natural", es_inflow: Double){

  def recoveryPropensity(es_inflow: Double) = Double {
    this.cover match {
      case "Degraded" => // function here
      case _ => 0
    }
  }

  def degradationPropensity(es_inflow: Double) = Double {
    this.cover match {
      case "Natural" => // function here
      case _ => 0
    }
  }

  def fertilityLossPropensity(es_inflow: Double) = Double {
    this.cover match {
      case "LowIntensityAgriculture" => // function here
      case "HighIntensityAgriculture" => // function here
      case _ => 0
    }
  }

  // method to update the land cover of a land unit
  def updateCover: Option[EcologicalUnit] = ...


}
