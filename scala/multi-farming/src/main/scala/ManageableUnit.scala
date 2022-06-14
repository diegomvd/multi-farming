case class ManageableUnit(composition: ParMap[Int, EcologicalUnit]{

  // this is poor, need to find a way to break out of the loop
  // the function is needed to check if the unit can be converted, while it has
  // at least one cultivated cell it can't
  def isAvailable() = Bool {
    this.composition.foreach{ _.cover match {
      case "LowIntensityAgriculture" => true
      case "HighIntensityAgriculture" => true
      }
    }
  }

  def conversionPropensity(resource_demand: Double) = Double {
    this.isAvailable() match {
      case true => // function here
      case false => 0
    }
  }
}
