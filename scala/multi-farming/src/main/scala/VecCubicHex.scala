import scala.math.sqrt
import scala.math.pow

final case class VecCubicHex(q: Int, r: Int, s: Int){

  // Returns the vector norm
  def norm(): Double = sqrt(pow(q,2) + pow(r,2) + pow(s,2))

  // Returns the normalized vector
  def normVec(): VecCubicHex = copy(q/norm(this),r/norm(this),s/norm(this))
  
}
