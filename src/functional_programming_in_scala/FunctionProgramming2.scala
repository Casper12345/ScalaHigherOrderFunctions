package functional_programming_in_scala


class FunctionProgramming2 {

  // exercise 2.1
  def fib(n: Int): Int = {
    def helper(acc: Int, i: Int): Int = {
      if (i > (n - 1) + (n - 2)) {
        acc
      } else {
        helper(acc + 1, i + 1)
      }
    }

    helper(0, 0)
  }

  // exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def helper(as: Array[A], i: Int): Boolean = {
      if (i > as.length - 2) {
        true
      } else {
        if (ordered(as(i), as(i + 1))) {
          helper(as, i + 1)
        } else {
          false
        }
      }
    }

    helper(as, 0)
  }

  // exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  // exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)

  }

  // exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }


}

object Main extends App {
  val f = new FunctionProgramming2

  println(f.isSorted(Array(1, 2, 3, 9, 1), (a: Int, b: Int) => a < b))

  println(f.fib(6))

  val function: Int => Int => Int = f.curry((a: Int, b: Int) => b + a)

  println(function(23)(2))


}