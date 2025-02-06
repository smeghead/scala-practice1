package test.S99

import S99.WorkingWithLists._

class WorkingWithListsTest extends munit.FunSuite {
  test("P01 (*) Find the last element of a list.") {
    assertEquals(8, last(List(1, 1, 2, 3, 5, 8)))
  }

  test("P02 (*) Find the last but one element of a list.") {
    assertEquals(5, penultimate(List(1, 1, 2, 3, 5, 8)))
  }
}
