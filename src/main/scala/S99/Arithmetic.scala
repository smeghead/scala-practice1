package S99

import scala.util.Random

object Arithmetic {
  // P31 (**) Determine whether a given integer number is prime.
  extension (n: Int) {
    def isPrime: Boolean = n match {
      case 1 => false
      case 2 => true
      case _ => (2 to (math.sqrt(n).toInt + 1)).toList.forall(n % _ != 0)
    }
  }
}
