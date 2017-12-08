package functional_programming_in_scala.Chapter3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[A](h: A, t: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  // exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t

  }

  // exercise 3.3
  def setHead[A](l: List[A], i: A): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(i, t)
  }

  // exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      n match {
        case 1 => t
        case _ => drop(t, n - 1)
      }

  }
  // exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(h, t) =>
        if (f(h)) {
          Cons(h, dropWhile(t,f))
        } else {
          dropWhile(t, f)
        }

  }

  // exercise 3.6
  def init[A](l: List[A]): List[A] = {
      Nil

  }
}



