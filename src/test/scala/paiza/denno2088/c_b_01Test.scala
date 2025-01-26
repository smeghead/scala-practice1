package test.paiza.denno2088

import paiza.denno2088.c_b_01

class c_b_01Test extends munit.FunSuite {
  test("面の初期化") {
    val board = c_b_01.createBoard(Array(".#.", "###", ".#."))

    assertEquals(c_b_01.exist(board, c_b_01.Point(0, 0)), true)
    assertEquals(c_b_01.exist(board, c_b_01.Point(0, 1)), false)
    assertEquals(c_b_01.exist(board, c_b_01.Point(0, 2)), true)
    assertEquals(c_b_01.exist(board, c_b_01.Point(1, 0)), false)
    assertEquals(c_b_01.exist(board, c_b_01.Point(1, 1)), false)
    assertEquals(c_b_01.exist(board, c_b_01.Point(1, 2)), false)
    assertEquals(c_b_01.exist(board, c_b_01.Point(2, 0)), true)
    assertEquals(c_b_01.exist(board, c_b_01.Point(2, 1)), false)
    assertEquals(c_b_01.exist(board, c_b_01.Point(2, 2)), true)
  }

  test("探索開始地点のリストアップ") {
    val board = c_b_01.createBoard(Array(".#.", "#.#", ".#."))

    val points = c_b_01.startPoints(board)

    assertEquals(points.length, 2)

    assertEquals(points(0), c_b_01.Point(0, 0))
    assertEquals(points(1), c_b_01.Point(1, 1))
  }
}
