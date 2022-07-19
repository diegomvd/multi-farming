/**
This object defines all the functions needed to manipulate the modulo coordinates
in the hexagonal lattice. This is used in EcoLandscape to create the ecological
units in different positions and define their neighborhood to create the composition
graph of the biophysical landscape.
*/

object ModCo{

  def apply(radius: Int) = List[Int] {
   val area = 3 * radius * radius + 3 * radius + 1
   (0 until area).map( _ ).toList
  }

  def toCubic(mod: Int, radius: Int) = (Int,Int) {
    // helper values
    val shift = 3 * radius + 2
    val ms = (mod + radius) / shift
    val mcs = (mod + 2) * radius / (shift - 1)

    // Need to make sure of this formula and check what is each coordinate
    // spatially
    val q = ms * (radius + 1) + mcs * radius
    val r = m + ms * (-2 * radius - 1) + mcs * (-radius - 1 )

    (q,r)
  }

  def toModulo(cub: (Int,Int), radius: Int) = Int {
    // helper values
    val area = 3 * radius * radius + 3 * radius + 1
    val shift = 3 * radius + 2
    val div = cub(0) + shift * cub(1)

    // "%" operator is the remainder operator in scala, with the function below we
    // obtain the modulo. Need to make sure this is right
    ( (div % area) + area ) % area
  }

  def neighbors(mod: Int, radius: Int, threshold: Int) = List[Int] {

    val cub = toCubic(mod,radius)

    val qfrom = cub(0) - threshold
    val qto = cub(0) + threshold
    val rfrom = cub(1) - threshold
    val rto = cub(1) + threshold

    (qfrom to qto).flatMap{ q =>
      (rfrom to rto).map( toModulo((q,_),radius) )
    }.toList
  }
}
