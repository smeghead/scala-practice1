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
}
