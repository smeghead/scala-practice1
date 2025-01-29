package test.paiza.denno2088

import paiza.denno2088.c_a_01

class c_a_01Test extends munit.FunSuite {
  test("面の初期化") {
    val board = c_a_01.createBoard(Array(
      "A....",
      ".#.#.",
      "....B"
    ))

    assertEquals(c_a_01.getCell(board, c_a_01.Point(0, 0)), Some(c_a_01.Cell(c_a_01.Point(0, 0), 0, false, false)))
    assertEquals(c_a_01.getCell(board, c_a_01.Point(1, 0)), Some(c_a_01.Cell(c_a_01.Point(1, 0), Int.MaxValue, false, false)))
    assertEquals(c_a_01.getCell(board, c_a_01.Point(2, 0)), Some(c_a_01.Cell(c_a_01.Point(2, 0), Int.MaxValue, false, false)))
    assertEquals(c_a_01.getCell(board, c_a_01.Point(3, 0)), Some(c_a_01.Cell(c_a_01.Point(3, 0), Int.MaxValue, false, false)))
    assertEquals(c_a_01.getCell(board, c_a_01.Point(4, 0)), Some(c_a_01.Cell(c_a_01.Point(4, 0), Int.MaxValue, false, false)))

    assertEquals(c_a_01.getCell(board, c_a_01.Point(0, 1)), Some(c_a_01.Cell(c_a_01.Point(0, 1), Int.MaxValue, false, false)))
    assertEquals(c_a_01.getCell(board, c_a_01.Point(1, 1)), null)
    assertEquals(c_a_01.getCell(board, c_a_01.Point(2, 1)), Some(c_a_01.Cell(c_a_01.Point(2, 1), Int.MaxValue, false, false)))
    assertEquals(c_a_01.getCell(board, c_a_01.Point(3, 1)), null)
    assertEquals(c_a_01.getCell(board, c_a_01.Point(4, 1)), Some(c_a_01.Cell(c_a_01.Point(4, 1), Int.MaxValue, false, false)))

    assertEquals(c_a_01.getCell(board, c_a_01.Point(0, 2)), Some(c_a_01.Cell(c_a_01.Point(0, 2), Int.MaxValue, false, false)))
    assertEquals(c_a_01.getCell(board, c_a_01.Point(1, 2)), Some(c_a_01.Cell(c_a_01.Point(1, 2), Int.MaxValue, false, false)))
    assertEquals(c_a_01.getCell(board, c_a_01.Point(2, 2)), Some(c_a_01.Cell(c_a_01.Point(2, 2), Int.MaxValue, false, false)))
    assertEquals(c_a_01.getCell(board, c_a_01.Point(3, 2)), Some(c_a_01.Cell(c_a_01.Point(3, 2), Int.MaxValue, false, false)))
    assertEquals(c_a_01.getCell(board, c_a_01.Point(4, 2)), Some(c_a_01.Cell(c_a_01.Point(4, 2), Int.MaxValue, false, true)))
  }

  test("面の描画") {
    val board = c_a_01.createBoard(Array(
      "A....",
      ".#.#.",
      "....B"
    ))

    assertEquals(c_a_01.display(board), "0****\n*#*#*\n****G")
  }

  test("一番小さい値のCellを取得する") {
    val board = c_a_01.createBoard(Array(
      "A....",
      ".#.#.",
      "....B"
    ))
    val cell = c_a_01.getSmallestCell(board)

    assertEquals(cell.p, c_a_01.Point(0, 0))
  }

  // test("探索") {
  //   val board = c_a_01.createBoard(Array(
  //     "A....",
  //     ".#.#.",
  //     "....B"
  //   ))
  //   val board2 = c_a_01.seek(board)

  //   assertEquals(c_a_01.display(board2), "01***\n1#*#*\n****G")
  //   assertEquals(c_a_01.getCell(board2, c_a_01.Point(0, 0)).map(_.fixed).getOrElse(false), true)
  // }
}
