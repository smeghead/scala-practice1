package test.S99

import S99.Arithmetic._

class ArithmeticTest extends munit.FunSuite {
  test("P31 (**) Determine whether a given integer number is prime.") {
    // assertEquals(true, 2.isPrime)
    assertEquals(true, 3.isPrime)
    assertEquals(false, 4.isPrime)
    assertEquals(true, 5.isPrime)
    assertEquals(false, 6.isPrime)
    assertEquals(true, 7.isPrime)
    assertEquals(false, 8.isPrime)
  }

  test("P32 (**) Determine the greatest common divisor of two positive integer numbers.") {
    assertEquals(
      3,
      gcd(6, 9)
    )
    assertEquals(
      9,
      gcd(36, 63)
    )
  }

  test("P33 (*) Determine whether two positive integer numbers are coprime.") {
    assertEquals(
      true,
      35.isCoprimeTo(64)
    )
    assertEquals(
      false,
      32.isCoprimeTo(64)
    )
  }

  test("P34 (**) Calculate Euler’s totient function ϕ(m).") {
    assertEquals(4, 10.totient)
  }

  test("P35 (**) Determine the prime factors of a given positive integer. ") {
    assertEquals(
      List(3, 3, 5, 7),
      315.primeFactors
    )
  }

  test("P36 (**) Determine the prime factors of a given positive integer (2).") {
    assertEquals(
      List((3,2), (5,1), (7,1)),
      315.primeFactorMultiplicity
    )
  }

  test("P37 (**) Calculate Euler’s totient function ϕ(m) (improved).") {
    assertEquals(
      6,
      totientImproved(9)
    )
    assertEquals(
      4,
      totientImproved(10)
    )
  }

  test("P38 (*) Compare the two methods of calculating Euler’s totient function.") {
    assertEquals(
      totient(10090),
      totientImproved(10090)
    )
  }

  test("P39 (*) A list of prime numbers.") {
    assertEquals(
      List(7, 11, 13, 17, 19, 23, 29, 31),
      listPrimesinRange(7 to 31)
    )
  }

  test("P40 (**) Goldbach’s conjecture.") {
    assertEquals(
      (5,23),
      28.goldbach
    )
    assertEquals(
      (5,7),
      12.goldbach
    )
  }
}
