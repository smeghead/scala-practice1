package test.S99

import S99.WorkingWithLists._

class WorkingWithListsTest extends munit.FunSuite {
  test("P01 (*) Find the last element of a list.") {
    assertEquals(8, last(List(1, 1, 2, 3, 5, 8)))
    assertEquals("c", last(List("a", "b", "c")))
  }

  test("P02 (*) Find the last but one element of a list.") {
    assertEquals(5, penultimate(List(1, 1, 2, 3, 5, 8)))
    assertEquals("b", penultimate(List("a", "b", "c")))
  }

  test("P03 (*) Find the Kth element of a list.") {
    assertEquals(1, nth(0, List(1, 1, 2, 3, 5, 8)))
    assertEquals(2, nth(2, List(1, 1, 2, 3, 5, 8)))
    assertEquals(5, nth(4, List(1, 1, 2, 3, 5, 8)))
    assertEquals(8, nth(5, List(1, 1, 2, 3, 5, 8)))
    assertEquals("a", nth(0, List("a", "b", "c")))
    assertEquals("b", nth(1, List("a", "b", "c")))
    assertEquals("c", nth(2, List("a", "b", "c")))
    intercept[NoSuchElementException] {
      nth(-1, List(1, 1, 2, 3, 5, 8))
    }
    intercept[NoSuchElementException] {
      nth(10, List(1, 1, 2, 3, 5, 8))
    }
  }

  test("P04 (*) Find the number of elements of a list.") {
    assertEquals(6, length(List(1, 1, 2, 3, 5, 8)))
    assertEquals(0, length(List()))
    assertEquals(3, length(List("b", "c", "d")))
  }

  test("P05 (*) Reverse a list.") {
    assertEquals(List(8, 5, 3, 2, 1, 1), reverse(List(1, 1, 2, 3, 5, 8)))
    assertEquals(List(), reverse(List()))
    assertEquals(List("d", "c", "b"), reverse(List("b", "c", "d")))
  }

  test("P06 (*) Find out whether a list is a palindrome.") {
    assertEquals(true, isPalindrome(List(1, 2, 3, 2, 1)))
    assertEquals(false, isPalindrome(List(1, 2, 3, 2, 1, 1)))
    assertEquals(true, isPalindrome(List(1)))
  }

  test("P07 (**) Flatten a nested list structure.") {
    assertEquals(List(1, 1, 2, 3, 5, 8), flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
  }
}
