package test.S99

import S99.LogicAndCodes._

class LogicAndCodesTest extends munit.FunSuite {
  test("P46 (**) Truth tables for logical expressions.") {
    assertEquals(true, and(true, true))
    assertEquals(false, and(true, false))

    assertEquals(false, xor(true, true))
    assertEquals(true, xor(false, true))

    assertEquals(
      List(
        "A     B     result",
        "true  true  true",
        "true  false true",
        "false true  false",
        "false false false",
      ),
      table2((a, b) => and(a, or(a, b))))
  }

}
