package test.S99

import S99.P01._

class P01Test extends munit.FunSuite {
  test("P01 (*) Find the last element of a list.") {
    assertEquals(8, last(List(1, 1, 2, 3, 5, 8)))
  }
}
