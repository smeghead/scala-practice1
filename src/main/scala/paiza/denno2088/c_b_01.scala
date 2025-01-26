package paiza.denno2088

object c_b_01 {
    case class Board(n: Int, matrix: Array[Array[Boolean]])

    def createBoard(lines: Array[String]): Board = {
        val matrix = lines.map(_.split("").map(_ == "."))
        Board(lines.length, matrix)
    }

    def exist(b: Board, x: Int, y: Int): Boolean = b.matrix(x)(y)
}
