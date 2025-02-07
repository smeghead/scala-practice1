package S99

object WorkingWithLists {
  // P01  (*) Find the last element of a list.
  def last[A](xs: List[A]): A = xs match {
    case h :: Nil => h
    case _ :: tail => last(tail)
    case _ => throw new NoSuchElementException
  }

  // def last[A](xs: List[A]): A = xs.last

  // def last(xs: List[Int]): Int = {
  //   xs.reverse.head
  // }

  // P02 (*) Find the last but one element of a list.
  def penultimate[A](xs: List[A]): A = xs match {
    case head :: _ :: Nil => head
    case _ :: tail => penultimate(tail)
    case _ => throw new NoSuchElementException
  }

  // P03 (*) Find the Kth element of a list.
  // def nth[A](n: Int, xs: List[A]): A = xs(n)

  // def nth[A](n: Int, ls: List[A]): A = 
  //   if (n >= 0) ls(n) 
  //   else throw new NoSuchElementException

  def nth[A](n: Int, xs: List[A]): A = (n, xs) match {
    case (0, head :: _) => head
    case (n, _ :: tail) => nth(n - 1, tail)
    case _ => throw new NoSuchElementException
  }
}
