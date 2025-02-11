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

  test("P08 (**) Eliminate consecutive duplicates of list elements.") {
    assertEquals(List("a", "b", "c", "a", "d", "e"), compress(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")))
  }

  test("P09 (**) Pack consecutive duplicates of list elements into sublists.") {
    assertEquals(
      List(List("a", "a", "a", "a"), List("b"), List("c", "c"), List("a", "a"), List("d"), List("e", "e", "e", "e")),
      pack(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))
    )
  }

  test("P10 (*) Run-length encoding of a list.") {
    assertEquals(
      List((4,"a"), (1,"b"), (2,"c"), (2,"a"), (1,"d"), (4,"e")),
       encode(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))
    )
  }

  test("P11 (*) Modified run-length encoding.") {
    assertEquals(
      List((4,"a"), "b", (2,"c"), (2,"a"), "d", (4,"e")),
      encodeModified(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))
    )
  }

  test("P12 (**) Decode a run-length encoded list.") {
    assertEquals(
      List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"),
      decode(List((4, "a"), (1, "b"), (2, "c"), (2, "a"), (1, "d"), (4, "e")))
    )
  }

  test("P13 (**) Run-length encoding of a list (direct solution).") {
    assertEquals(
      List((4,"a"), (1,"b"), (2,"c"), (2,"a"), (1,"d"), (4,"e")),
       encodeDirect(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))
    )
  }

  test("P14 (*) Duplicate the elements of a list.") {
    assertEquals(
      List("a", "a", "b", "b", "c", "c", "c", "c", "d", "d"),
      duplicate(List("a", "b", "c", "c", "d"))
    )
  }

  
  test("P15 (**) Duplicate the elements of a list a given number of times.") {
    assertEquals(
      List("a", "a", "a", "b", "b", "b", "c", "c", "c", "c", "c", "c", "d", "d", "d"),
      duplicateN(3, List("a", "b", "c", "c", "d"))
    )
  }

}
