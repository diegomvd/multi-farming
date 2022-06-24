case class PropensityHub(recovery: ParMap[ModuloCoord,Double],
                         degradation: ParMap[ModuloCoord,Double],
                         fertility_loss: ParMap[ModuloCoord,Double],
                         conversion: ParMap[Int,Double],
                         birth: Double,
                         death: Double){

  def totalSpontaneous(): Double =
    this.recovery.values.sum + this.degradation.values.sum + this.fertility_loss.values.sum

  def totalConversion()  

}

object PropensityHub{

  def totalSpontaneous()



}
