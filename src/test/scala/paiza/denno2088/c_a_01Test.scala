package test.paiza.denno2088

import paiza.denno2088.c_a_01

class c_a_01Test extends munit.FunSuite {
  test("面の初期化") {
    val board = c_a_01.createBoard(Array(
      "A....",
      ".#.#.",
      "....B"
    ))

    assertEquals(c_a_01.getCell(board, c_a_01.Point(0, 0)), Some(c_a_01.Cell(0, false, null, false)))
    assertEquals(c_a_01.getCell(board, c_a_01.Point(1, 0)), Some(c_a_01.Cell(Int.MaxValue, false, null, false)))
    assertEquals(c_a_01.getCell(board, c_a_01.Point(2, 0)), Some(c_a_01.Cell(Int.MaxValue, false, null, false)))
    assertEquals(c_a_01.getCell(board, c_a_01.Point(3, 0)), Some(c_a_01.Cell(Int.MaxValue, false, null, false)))
    assertEquals(c_a_01.getCell(board, c_a_01.Point(4, 0)), Some(c_a_01.Cell(Int.MaxValue, false, null, false)))
  }
}
