package test.paiza.denno2088

import paiza.denno2088.c_b_01

class c_b_01Test extends munit.FunSuite {
  test("面の初期化") {
    val board = c_b_01.createBoard(Array(".#.", "###", ".#."))

    assertEquals(c_b_01.exist(board, 0, 0), true)
    assertEquals(c_b_01.exist(board, 0, 1), false)
    assertEquals(c_b_01.exist(board, 0, 2), true)
    assertEquals(c_b_01.exist(board, 1, 0), false)
    assertEquals(c_b_01.exist(board, 1, 1), false)
    assertEquals(c_b_01.exist(board, 1, 2), false)
    assertEquals(c_b_01.exist(board, 2, 0), true)
    assertEquals(c_b_01.exist(board, 2, 1), false)
    assertEquals(c_b_01.exist(board, 2, 2), true)
  }
}
