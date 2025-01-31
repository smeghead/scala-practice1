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
    assertEquals(c_a_01.getCell(board, c_a_01.Point(1, 1)), None)
    assertEquals(c_a_01.getCell(board, c_a_01.Point(2, 1)), Some(c_a_01.Cell(c_a_01.Point(2, 1), Int.MaxValue, false, false)))
    assertEquals(c_a_01.getCell(board, c_a_01.Point(3, 1)), None)
    assertEquals(c_a_01.getCell(board, c_a_01.Point(4, 1)), Some(c_a_01.Cell(c_a_01.Point(4, 1), Int.MaxValue, false, false)))

    assertEquals(c_a_01.getCell(board, c_a_01.Point(0, 2)), Some(c_a_01.Cell(c_a_01.Point(0, 2), Int.MaxValue, false, false)))
    assertEquals(c_a_01.getCell(board, c_a_01.Point(1, 2)), Some(c_a_01.Cell(c_a_01.Point(1, 2), Int.MaxValue, false, false)))
    assertEquals(c_a_01.getCell(board, c_a_01.Point(2, 2)), Some(c_a_01.Cell(c_a_01.Point(2, 2), Int.MaxValue, false, false)))
    assertEquals(c_a_01.getCell(board, c_a_01.Point(3, 2)), Some(c_a_01.Cell(c_a_01.Point(3, 2), Int.MaxValue, false, false)))
    assertEquals(c_a_01.getCell(board, c_a_01.Point(4, 2)), Some(c_a_01.Cell(c_a_01.Point(4, 2), Int.MaxValue, false, true)))
  }

  test("ゴールの拡張") {
    val board = c_a_01.createBoard(Array(
      "A....",
      ".#.#.",
      "....B"
    ))
    val b2 = c_a_01.spreadGoal(board)

    println(c_a_01.display(b2))
    assertEquals(c_a_01.getCell(b2, c_a_01.Point(0, 0)), Some(c_a_01.Cell(c_a_01.Point(0, 0), 0, false, false)))
    assertEquals(c_a_01.getCell(b2, c_a_01.Point(1, 0)), Some(c_a_01.Cell(c_a_01.Point(1, 0), Int.MaxValue, false, false)))
    assertEquals(c_a_01.getCell(b2, c_a_01.Point(2, 0)), Some(c_a_01.Cell(c_a_01.Point(2, 0), Int.MaxValue, false, false)))
    assertEquals(c_a_01.getCell(b2, c_a_01.Point(3, 0)), Some(c_a_01.Cell(c_a_01.Point(3, 0), Int.MaxValue, false, false)))
    assertEquals(c_a_01.getCell(b2, c_a_01.Point(4, 0)), Some(c_a_01.Cell(c_a_01.Point(4, 0), Int.MaxValue, false, true)))

    assertEquals(c_a_01.getCell(b2, c_a_01.Point(0, 1)), Some(c_a_01.Cell(c_a_01.Point(0, 1), Int.MaxValue, false, false)))
    assertEquals(c_a_01.getCell(b2, c_a_01.Point(1, 1)), None)
    assertEquals(c_a_01.getCell(b2, c_a_01.Point(2, 1)), Some(c_a_01.Cell(c_a_01.Point(2, 1), Int.MaxValue, false, false)))
    assertEquals(c_a_01.getCell(b2, c_a_01.Point(3, 1)), None)
    assertEquals(c_a_01.getCell(b2, c_a_01.Point(4, 1)), Some(c_a_01.Cell(c_a_01.Point(4, 1), Int.MaxValue, false, true)))

    assertEquals(c_a_01.getCell(b2, c_a_01.Point(0, 2)), Some(c_a_01.Cell(c_a_01.Point(0, 2), Int.MaxValue, false, true)))
    assertEquals(c_a_01.getCell(b2, c_a_01.Point(1, 2)), Some(c_a_01.Cell(c_a_01.Point(1, 2), Int.MaxValue, false, true)))
    assertEquals(c_a_01.getCell(b2, c_a_01.Point(2, 2)), Some(c_a_01.Cell(c_a_01.Point(2, 2), Int.MaxValue, false, true)))
    assertEquals(c_a_01.getCell(b2, c_a_01.Point(3, 2)), Some(c_a_01.Cell(c_a_01.Point(3, 2), Int.MaxValue, false, true)))
    assertEquals(c_a_01.getCell(b2, c_a_01.Point(4, 2)), Some(c_a_01.Cell(c_a_01.Point(4, 2), Int.MaxValue, false, true)))
  }

  test("面の描画") {
    val board = c_a_01.createBoard(Array(
      "A....",
      ".#.#.",
      "....B"
    ))

    assertEquals(c_a_01.display(board), "0- *- *- *- *- \n*- ## *- ## *- \n*- *- *- *- G- ")
  }

  test("一番小さい値のCellを取得する") {
    val board = c_a_01.createBoard(Array(
      "A....",
      ".#.#.",
      "....B"
    ))
    val cell = c_a_01.getSmallestCell(board)

    assertEquals(cell.point, c_a_01.Point(0, 0))
  }

  test("指定したセルを確定させる") {
    val board = c_a_01.createBoard(Array(
      "A....",
      ".#.#.",
      "....B"
    ))
    val board2 = c_a_01.updateCell(board, c_a_01.Point(0, 0), c => c.copy(fixed = true))

    assertEquals(c_a_01.display(board2), "0+ *- *- *- *- \n*- ## *- ## *- \n*- *- *- *- G- ")
    assertEquals(c_a_01.getCell(board2, c_a_01.Point(0, 0)).map(_.fixed).getOrElse(false), true)
  }

  test("指定したセルの値を更新する") {
    val board = c_a_01.createBoard(Array(
      "A....",
      ".#.#.",
      "....B"
    ))
    val board2 = c_a_01.updateCell(board, c_a_01.Point(1, 0), c => c.copy(value = 1))

    assertEquals(c_a_01.display(board2), "0- 1- *- *- *- \n*- ## *- ## *- \n*- *- *- *- G- ")
    assertEquals(c_a_01.getCell(board2, c_a_01.Point(1, 0)).map(_.value).getOrElse(-1), 1)
  }

  test("未確定のうち一番小さい値のCellを取得する") {
    val board = c_a_01.createBoard(Array(
      "A....",
      ".#.#.",
      "....B"
    ))
    val b2 = c_a_01.updateCell(board, c_a_01.Point(0, 0), c => c.copy(fixed = true))
    val b3 = c_a_01.updateCell(b2, c_a_01.Point(1, 0), c => c.copy(value = 1))

    val cell = c_a_01.getUnfixedSmallestCell(b3)

    assertEquals(cell.point, c_a_01.Point(1, 0))
  }

  test("隣りあうCellを取得する 0 0") {
    val board = c_a_01.createBoard(Array(
      "A....",
      ".#.#.",
      "....B"
    ))

    val cells = c_a_01.getNeighborhoodCells(board, c_a_01.Point(0, 0))

    assertEquals(cells(0).point, c_a_01.Point(1, 0))
  }

  test("探索") {
    val board = c_a_01.createBoard(Array(
      "A....",
      ".#.#.",
      "....B"
    ))
    val b2 = c_a_01.seek(board)

    assertEquals(c_a_01.display(b2), "0+ 1- *- *- *- \n1- ## *- ## *- \n*- *- *- *- G- ")
    assertEquals(c_a_01.getCell(b2, c_a_01.Point(0, 0)), Some(c_a_01.Cell(c_a_01.Point(0, 0), 0, true, false)))
    assertEquals(c_a_01.getCell(b2, c_a_01.Point(0, 0)).map(_.fixed).getOrElse(false), true)
  }

  test("全て確定してない") {
    val board = c_a_01.createBoard(Array(
      "A.",
      ".B",
    ))
    val b2 = c_a_01.seek(board)
    val b3 = c_a_01.updateCell(b2, c_a_01.Point(0, 0), c => c.copy(fixed = true))
    val b4 = c_a_01.updateCell(b3, c_a_01.Point(1, 0), c => c.copy(fixed = true))
    val b5 = c_a_01.updateCell(b4, c_a_01.Point(0, 1), c => c.copy(fixed = true))

    assertEquals(c_a_01.allFixed(b5), false)
  }

  test("全て確定している") {
    val board = c_a_01.createBoard(Array(
      "A.",
      ".B",
    ))
    val b2 = c_a_01.seek(board)
    val b3 = c_a_01.updateCell(b2, c_a_01.Point(0, 0), c => c.copy(fixed = true))
    val b4 = c_a_01.updateCell(b3, c_a_01.Point(1, 0), c => c.copy(fixed = true))
    val b5 = c_a_01.updateCell(b4, c_a_01.Point(0, 1), c => c.copy(fixed = true))
    val b6 = c_a_01.updateCell(b5, c_a_01.Point(1, 1), c => c.copy(fixed = true))

    assertEquals(c_a_01.allFixed(b6), true)
  }
}
