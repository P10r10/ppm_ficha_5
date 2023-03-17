
//Ficha5.Example.scala (Case Class and Companion Object)
sealed trait MyTree[+A]

case object Empty extends MyTree[Nothing]

case class Node[A](value: A, left: MyTree[A], right: MyTree[A]) extends MyTree[A]

case class Example[A](myField: MyTree[A]) {
  def maximum() = Example.maximum(this.myField.asInstanceOf[MyTree[Int]])

  def depth() = Example.depth(this.myField)

  def map[B](f: A => B): MyTree[B] = Example.map(this.myField)(f)
}

object Example {
  def maximum(t: MyTree[Int]): Option[Int] = t match {
    case Empty => None
    case Node(value, left, right) =>
      (maximum(left), maximum(right)) match {
        case (None, None) => Some(value)
        case (None, Some(a)) => Some(value max a)
        case (Some(a), None) => Some(value max a)
        case (Some(a), Some(b)) => Some(a max b max value)
      }
  }

  def depth[A](t: MyTree[A]): Int = t match {
    case Empty => 0
    case Node(_, left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](t: MyTree[A])(f: A => B): MyTree[B] = t match {
    case Empty => Empty
    case Node(value, left, right) => Node(f(value), map(left)(f), map(right)(f))
  }

  def main(args: Array[String]): Unit = {
    val tree1 = Node(42, Node(8, Empty, Empty), Node(10, Node(9, Empty, Empty), Empty))
    val t = Example(tree1)
    println(s"Maximum element of the tree: ${t.maximum()}")
    println(s"Depth of the tree: ${t.depth()}")
    println(s"Map: ${t.map((x: Int) => x * 2)}")
  }
}