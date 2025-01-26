package paiza.denno2088

object c_b_01 {
    case class Board(n: Int, matrix: Array[Array[Boolean]])

    case class Point(x: Int, y: Int)

    def createBoard(lines: Array[String]): Board = {
        val matrix = lines.map(_.split("").map(_ == "."))
        Board(lines.length, matrix)
    }

    def exist(b: Board, p: Point): Boolean = b.matrix(p.x)(p.y)

    def startPoints(b: Board): IndexedSeq[Point] = {
        val points = for (
            x <- 0 until b.n - 1;
            y <- 0 until b.n - 1
        ) yield Point(x, y)
        points.filter(p => exist(b, p))
    }
}
