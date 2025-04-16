package test.S99

import S99.LogicAndCodes._

class LogicAndCodesTest extends munit.FunSuite {
  // test("P46 (**) Truth tables for logical expressions.") {
  //   assertEquals(true, and(true, true))
  //   assertEquals(false, and(true, false))

  //   assertEquals(false, xor(true, true))
  //   assertEquals(true, xor(false, true))

  //   assertEquals(
  //     List(
  //       "A     B     result",
  //       "true  true  true",
  //       "true  false true",
  //       "false true  false",
  //       "false false false",
  //     ),
  //     table2((a, b) => and(a, or(a, b)))
  //   )
  // }

  test("P47 (*) Truth tables for logical expressions (2).") {
    assertEquals(
      List(
        "A     B     result",
        "true  true  true",
        "true  false true",
        "false true  false",
        "false false false",
      ),
      table2((a, b) => a `and` (a `or` not(b)))
    )
  }

  test("P49 (**) Gray code.") {
    assertEquals(
      List("0", "1"),
      gray(1)
    )
    assertEquals(
      List("00", "01", "11", "10"),
      gray(2)
    )
    assertEquals(
      List("000", "001", "011", "010", "110", "111", "101", "100"),
      gray(3)
    )
  }

  test("P50 (***) Huffman code.") {
    assertEquals(
      List(("a","0"), ("b","101"), ("c","100"), ("d","111"), ("e","1101"), ("f","1100")),
      huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))
    )
  }
}
