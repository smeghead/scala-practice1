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

  // P04 (*) Find the number of elements of a list.
  // def length[A](xs: List[A]): Int = {
  //   def lengthRec[A](xs: List[A], acc: Int): Int = (xs, acc) match {
  //     case (Nil, acc) => acc
  //     case (_ :: tail, acc) => lengthRec(tail, acc + 1)
  //   }
  //   lengthRec(xs, 0)
  // }

  def length[A](xs: List[A]): Int = xs.foldLeft(0) { (acc, _) => acc + 1}

  // P05 (*) Reverse a list.
  // def reverse[A](xs: List[A]): List[A] = xs.reverse

  // def reverse[A](xs: List[A]): List[A] = xs.foldRight(List.empty) { (n, acc) => acc :+ n }
  def reverse[A](xs: List[A]): List[A] = xs.foldLeft(List.empty) { (acc, n) => n :: acc }

  // P06 (*) Find out whether a list is a palindrome.
  def isPalindrome[A](xs: List[A]): Boolean = {
    if (xs.isEmpty) {
      return true
    }
    if (xs.head != xs.last) {
      return false
    }
    isPalindrome(xs.slice(1, xs.length - 1))
  }

  // P07 (**) Flatten a nested list structure.
  // def flatten(xs: List[Any]): List[Any] = xs match {
  //   case head :: tail => if (head.isInstanceOf[List[Any]]) {
  //     flatten(head.asInstanceOf[List[Any]]) ::: flatten(tail)
  //   } else {
  //     List(head) ::: flatten(tail)
  //   }
  //   case Nil => List.empty
  // }

  def flatten(xs: List[Any]): List[Any] = xs match {
    case head :: tail if head.isInstanceOf[List[Any]] => flatten(head.asInstanceOf[List[Any]]) ::: flatten(tail)
    case head :: tail => List(head) ::: flatten(tail)
    case Nil => List.empty
  }

  // P08 (**) Eliminate consecutive duplicates of list elements.
  // def compress[A](xs: List[A]): List[A] = {
  //   def rec[A](xs: List[A], acc: List[A]): List[A] = xs match {
  //     case head :: second :: tail if head == second => rec(xs.drop(1), acc)
  //     case head :: second :: tail => rec(xs.drop(1), acc :+ xs.head)
  //     case head :: Nil => acc ::: List[A](xs.head)
  //     case Nil => throw new RuntimeException
  //   }
  //   rec(xs, List[A]())
  // }

  def compress[A](xs: List[A]): List[A] = {
    xs.foldLeft(List[A]()) { (acc, e) => 
      if (acc.isEmpty || acc.last != e) acc :+ e
      else acc
    }
  }

  // P09 (**) Pack consecutive duplicates of list elements into sublists.
  // def pack[A](xs: List[A]): List[Any] = {
  //   xs.foldLeft(List[List[A]]()) { (acc, e) => 
  //     if (acc.isEmpty || acc.last.head != e) acc.appended(List[A](e))
  //     else acc.updated(acc.length - 1, acc.last :+ e)
  //   }
  // }

  def pack[A](xs: List[A]): List[List[A]] = {
    if (xs.isEmpty) List(List())
    else {
      val (packed, next) = xs span { _ == xs.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  // P10 (*) Run-length encoding of a list.
  def encode[A](xs: List[A]): List[(Int, A)] = {
    if (xs.isEmpty) List()
    else {
      val (packed, next) = xs span { _ == xs.head }
      val encoded = (packed.length, packed.head)
      if (next == Nil) List(encoded)
      else encoded :: encode(next)
    }
  }

  // P11 (*) Modified run-length encoding.
  def encodeModified[A](xs: List[A]): List[A | (Int, A)] = encode(xs) map { (e: (Int, A)) => if (e._1 == 1) e._2 else e }

  // P12 (**) Decode a run-length encoded list.
  def decode[A](xs: List[(Int, A)]): List[A] = xs flatMap { (n, e) => List.fill(n)(e) }

  // P13 (**) Run-length encoding of a list (direct solution).
  def encodeDirect = encode // 問題を良く読まずに、既にP10でダイレクトにエンコードしたバージョンを書いていた。

  // P14 (*) Duplicate the elements of a list.
  def duplicate[A](xs: List[A]): List[A] = xs flatMap { List.fill(2)(_) }

  // P15 (**) Duplicate the elements of a list a given number of times.
  def duplicateN[A](n: Int, xs: List[A]): List[A] = xs flatMap { List.fill(n)(_) }
}
