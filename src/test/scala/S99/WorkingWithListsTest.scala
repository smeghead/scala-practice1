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

  test("P16 (**) Drop every N Nth element from a list.") {
    assertEquals(
      List("a", "b", "d", "e", "g", "h", "j", "k"),
      drop(3, List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"))
    )
  }

  test("P17 (*) Split a list into two parts.") {
    assertEquals(
      (List("a", "b", "c"),List("d", "e", "f", "g", "h", "i", "j", "k")),
      split(3, List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"))
    )
    assertEquals(
      (List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"), List()),
      split(100, List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"))
    )
  }

  test("P18 (**) Extract a slice from a list.") {
    assertEquals(
      List("d", "e", "f", "g"),
      slice(3, 7, List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"))
    )
  }

  test("P19 (**) Rotate a list N N places to the left.") {
    assertEquals(
      List("d", "e", "f", "g", "h", "i", "j", "k", "a", "b", "c"),
      rotate(3, List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"))
    )
    assertEquals(
      List("j", "k", "a", "b", "c", "d", "e", "f", "g", "h", "i"),
      rotate(-2, List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"))
    )
    assertEquals(
      List("d", "e", "f", "g", "h", "i", "j", "k", "a", "b", "c"),
      rotate(14, List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"))
    )
  }

  test("P20 (*) Remove the K Kth element from a list.") {
    assertEquals(
      (List("a", "c", "d"),"b"),
      removeAt(1, List("a", "b", "c", "d"))
    )
  }

  test("P21 (*) Insert an element at a given position into a list.") {
    assertEquals(
      List("a", "new", "b", "c", "d"),
      insertAt("new", 1, List("a", "b", "c", "d"))
    )
  }

  test("P22 (*) Create a list containing all integers within a given range.") {
    assertEquals(
      List(4, 5, 6, 7, 8, 9),
      range(4, 9)
    )
    assertEquals(
      List(4),
      range(4, 4)
    )
  }

  test("P23 (**) Extract a given number of randomly selected elements from a list.") {
    assertEquals(
      3,
      randomSelect(3, List("a", "b", "c", "d", "f", "g", "h")).length
    )
  }

  test("P24 (*) Lotto: Draw N different random numbers from the set 1..M.") {
    val result = lotto(6, 49)
    assertEquals(6, result.toSet.size)
    result map { (n: Int) =>
      assert(n > 0)
      assert(n <= 49)
    }
  }

  test("P25 (*) Generate a random permutation of the elements of a list.") {
    val original = List("a", "b", "c", "d", "e", "f")
    val result = randomPermute(original)
    assertEquals(6, result.toSet.size)
    result map { (s: String) =>
      assert(original.contains(s))
    }
  }

  test("P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.") {
    assertEquals(
      List(List("a", "b"), List("a", "c"), List("b", "c")),
      combinations(2, List("a", "b", "c"))
    )
  }

  test("P27 (**) Group the elements of a set into disjoint subsets.") {
    val result = group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
    assertEquals(
      result.length,
      1260
    )
    assertEquals(
      List(List("Aldo", "Beat"), List("Carla", "David", "Evi"), List("Flip", "Gary", "Hugo", "Ida")),
      result(0)
    )
  }

  test("P28 (**) Sorting a list of lists according to length of sublists.") {
    assertEquals(
      List(List("o"), List("d", "e"), List("d", "e"), List("m", "n"), List("a", "b", "c"), List("f", "g", "h"), List("i", "j", "k", "l")),
      lsort(List(List("a", "b", "c"), List("d", "e"), List("f", "g", "h"), List("d", "e"), List("i", "j", "k", "l"), List("m", "n"), List("o")))
    )
    assertEquals(
      List(List("i", "j", "k", "l"), List("o"), List("a", "b", "c"), List("f", "g", "h"), List("d", "e"), List("d", "e"), List("m", "n")),
      lsortFreq(List(List("a", "b", "c"), List("d", "e"), List("f", "g", "h"), List("d", "e"), List("i", "j", "k", "l"), List("m", "n"), List("o")))
    )
  }
}
