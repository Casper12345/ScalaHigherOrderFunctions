package functional_programming_in_scala.Chapter3

object Main {

  def main(args: Array[String]): Unit = {
    val l = List(1,2,3,4,5,6)
    println(List.tail(l))
    println(List.setHead(l, 230))
    println(List.drop(l, 3))
    println(List.dropWhile(l, (a: Int) => a > 2))
    println(List.init(l))
    println(List.length(l))
    println(List.map(l, (a: Int) => a.toString))
    println(List.filter(l, (x: Int) => x > 2))
  }

}
