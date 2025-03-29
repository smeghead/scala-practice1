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
  var primes = LazyList.cons(2, LazyList.from(3, 2).filter(_.isPrime))
  extension (n: Int) {
    def primeFactors: List[Int] = if (n == 1) Nil else {
      primes.find(n % _ == 0) match {
        case Some(x) => x :: (n / x).primeFactors
        case None => throw new NoSuchElementException
      }
    }
  }

  // // P36 (**) Determine the prime factors of a given posit{ive integer (2).
  def encode(xs: List[Int]): List[(Int, Int)] = {
    if (xs.isEmpty) List()
    else {
      val (packed, tail) = xs span { xs.head == _ }
      val encoded = (packed.head, packed.length)
      tail match {
        case Nil => List(encoded)
        case _ => encoded :: encode(tail)
      }
    }
  }
  extension (n: Int) {
    def primeFactorMultiplicity: List[(Int, Int)] = {
      encode(n.primeFactors sortWith { _ < _ })
    }
  }

  // P37 (**) Calculate Euler’s totient function ϕ(m) (improved).
  def totientImproved(n: Int): Int = {
    // n.primeFactorMultiplicity.map { e =>
    //   (e._1 - 1) * math.pow(e._1, e._2 - 1)
    // }.product.toInt

    n.primeFactorMultiplicity.map { _ match {
      case (p, m) => (p - 1) * math.pow(p, m - 1)
    }}.product.toInt
  }

  // P39 (*) A list of prime numbers.
  // def listPrimesinRange(xs: IndexedSeq[Int]): List[Int] = xs.filter(_.isPrime).toList
  def listPrimesinRange(r: Range): List[Int] = {
    primes.dropWhile(_ < r.start).takeWhile(_ <= r.end).toList
  }

  // P40 (**) Goldbach’s conjecture.
  // extension (n: Int) {
    // def goldbach: (Int, Int) = {
    //   def rec(ax: LazyList[Int], bx: LazyList[Int]): Option[(Int, Int)] = {
    //     (ax.head, bx.head) match {
    //       case (a, b) if a + b == n => Some((a, b))
    //       case (a, b) if a + b > n => None
    //       case (a, b) => {
    //         rec(ax, bx.tail) match {
    //           case Some((aa, bb)) => Some((aa, bb))
    //           case None => rec(ax.tail, bx)
    //         }
    //       }
    //     }
    //   }
    //   rec(primes, primes) match {
    //     case Some((aaa, bbb)) => (aaa, bbb)
    //     case None => throw new NoSuchElementException
    //   }
    // }
  // }
  extension (start: Int) {
    def goldbach: (Int, Int) = {
      primes takeWhile { _ < start } find { p => (start - p).isPrime } match {
        case None => throw new IllegalArgumentException
        case Some(p1) => (p1, start - p1)
      }
    }
  }

  // P41 (**) A list of Goldbach compositions.
  def getGoldbachList(r: Range): List[String] = {
    r.filter(_ % 2 == 0).map(n => {
      n.goldbach match {
        case (a, b) => "%d = %d + %d".format(n, a, b)
      }
    }).toList
  }
}
