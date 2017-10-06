class HigherOrderFunctions {

  /**
    * Implementation of map function.
    *
    * @param lst list of A
    * @param f   maps A to B
    * @tparam A unrestricted generic type  A
    * @tparam B unrestricted generic type  B
    * @return list of B
    */
  def map[A, B](lst: List[A], f: A => B): List[B] = lst match {
    case Nil => Nil
    case h :: t => f(h) :: map(t, f)
  }


  /**
    * Implementation of filter function.
    *
    * @param lst list of A
    * @param f   filter lambda expression
    * @tparam A unrestricted generic type A
    * @return list of A
    */
  def filter[A](lst: List[A], f: A => Boolean): List[A] = lst match {
    case Nil => Nil
    case h :: t => if (f(h)) h :: filter(t, f) else filter(t, f)
  }

  /**
    * Implementation of flatten function.
    *
    * @param lst list of A
    * @tparam A generic type A
    * @return list of A
    */
  def flatten[A](lst: List[List[A]]): List[A] = lst match {
    case Nil => Nil
    case h :: t => h ::: flatten(t)
  }

  /**
    * Implementation of fold right function...
    *
    * @param lst list of A
    * @param bc  base case of B
    * @param f   lambda maps A and B to B
    * @tparam A generic type A
    * @tparam B generic type B
    * @return generic type B  .
    */
  def foldRight[A, B](lst: List[A], bc: B, f: (A, B) => B): B = lst match {
    case Nil => bc
    case h :: t => f(h, foldRight(t, bc, f))
  }


}
