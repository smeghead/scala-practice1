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

  // P32 (**) Determine the greatest common divisor of two positive integer numbers.
  // def gcd(a: Int, b: Int): Int = {
  //   (math.min(a, b) to 2 by -1).find(i => a % i == 0 && b % i == 0) match {
  //     case Some(answer) => answer
  //     case None => throw new NoSuchElementException
  //   }
  // }
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
}
