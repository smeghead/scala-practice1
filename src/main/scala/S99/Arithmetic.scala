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

  // P33 (*) Determine whether two positive integer numbers are coprime.
  extension (n: Int) {
    def isCoprimeTo(other: Int): Boolean = gcd(n, other) == 1
  }

  // P34 (**) Calculate Euler’s totient function ϕ(m).
  extension (n: Int) {
    // def totient: Int = (1 to n).foldLeft(0) { (acc, e) => acc + (if (e.isCoprimeTo(n)) 1 else 0) }
    def totient: Int = (1 to n).filter(n.isCoprimeTo(_)).length
  }

  // P35 (**) Determine the prime factors of a given positive integer. 
  var primes = LazyList.cons(2, LazyList.from(3, 1).filter(_.isPrime))
  extension (n: Int) {
    def primeFactors: List[Int] = n match {
      case 1 => Nil
      case _ => {
        primes.find(n % _ == 0) match {
          case Some(x) => x :: (n / x).primeFactors
          case None => throw new NoSuchElementException
        }
      }
    }
  }

  // // P36 (**) Determine the prime factors of a given positive integer (2).
  // extension (n: Int) {
  //   def primeFactorMultiplicity: List((Int, Int)) = {
  //     def encode[A](xs: List[A]): List((Int, Int)) = {
  //       if (xs.isEmpty) List()
  //       else {
  //         val (packed, tail) = xs span { xs.head == _ }
  //         val encoded = (packed.head, packed.length)
  //         tail match {
  //           case Nil => List(encoded)
  //           case _ => encoded :: encode(tail)
  //         }
  //       }
  //     }
  //     encode(n.primeFactors)
  //   }
  // }
}
