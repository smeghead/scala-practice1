package S99

object LogicAndCodes {
  // P46 (**) Truth tables for logical expressions.
  // def and(a: Boolean, b: Boolean): Boolean = a && b
  // def or(a: Boolean, b: Boolean): Boolean = a || b
  def nand(a: Boolean, b: Boolean): Boolean = !(a && b)
  def nor(a: Boolean, b: Boolean): Boolean = !(a || b)
  def xor(a: Boolean, b: Boolean): Boolean = a != b

  def table2(f: (a: Boolean, b: Boolean) => Boolean): List[String] = {
    val table = for {
      x <- List(true, false)
      y <- List(true, false)
    } yield "%-6s%-6s%s".format(x, y, f(x, y))
    List("A     B     result") ++ table
  }
  // P47 (*) Truth tables for logical expressions (2).
  extension (a: Boolean) {
    def and(b: Boolean): Boolean = a && a
    def or(b: Boolean): Boolean = a || b
  }
  def not(a: Boolean): Boolean = !a

  // P49 (**) Gray code.
  def gray(n: Int): List[String] = {
    if (n == 0) List("")
    else {
      val lower = gray(n - 1)
      (lower map { "0" + _ }) ::: (lower.reverse map { "1" + _ })
    }
  }

  // P50 (***) Huffman code.
  private abstract sealed class Tree[A] {
    val freq: Int
    def toCode: List[(A, String)] = toCodePrefixed("")
    def toCodePrefixed(prefix: String): List[(A, String)]
  }
  private final case class InternalNode[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
    val freq: Int = left.freq + right.freq
    def toCodePrefixed(prefix: String): List[(A, String)] = {
      left.toCodePrefixed(prefix + "0") ::: right.toCodePrefixed(prefix + "1")
    }
  }
  private final case class LeafNode[A](element: A, freq: Int) extends Tree[A] {
    def toCodePrefixed(prefix: String): List[(A, String)] = List((element, prefix))
  }

  def huffman[A](ls: List[(A, Int)]): List[(A, String)] = {
    import collection.immutable.Queue
    def dequeueSmallest(q1: Queue[Tree[A]], q2: Queue[Tree[A]]) = {
      if (q2.isEmpty) (q1.front, q1.dequeue._2, q2)
      else if (q1.isEmpty || q2.front.freq < q1.front.freq) (q2.front, q1, q2.dequeue._2)
      else (q1.front, q1.dequeue._2, q2)
    }
    def haffmanR(q1: Queue[Tree[A]], q2: Queue[Tree[A]]): List[(A, String)] = {
      if (q1.length + q2.length == 1) (if (q1.isEmpty) q2.front else q1.front).toCode
      else {
        val (v1, q3, q4) = dequeueSmallest(q1, q2)
        val (v2, q5, q6) = dequeueSmallest(q3, q4)
        haffmanR(q5, q6.enqueue(InternalNode(v1, v2)))
      }
    }
    haffmanR(Queue.empty.enqueue(ls.sortWith((a, b) => a._2 < b._2).map(e => LeafNode(e._1, e._2))), Queue.empty)
  }
}
