sealed trait MyStream[+A] {
  def headOption: Option[A] = this match {
    case MyEmpty => None
    case MyCons(h, t) => Some(h())
  }

  def toListRecursive: List[A] = this match {
    case MyEmpty => Nil
    case MyCons(h, t) => h() :: t().toList
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(acc: List[A], st: MyStream[A]): List[A] = st match {
      case MyEmpty => acc
      case MyCons(h, t) => go(h() :: acc, t())
    }
    go(List.empty, this).reverse
  }

  def take(n: Int): MyStream[A] = this match {
    case MyEmpty                => MyEmpty
    case MyCons(_, _) if n <= 0 => MyEmpty
    case MyCons(h, t)           => MyCons(h, () => t().take(n-1))
  }

  def takeWhile(p: A => Boolean): MyStream[A] = this match {
    case MyCons(h, t) if p(h()) => MyCons(h, () => t() takeWhile p )
    case _                      => MyEmpty
  }
}

case object MyEmpty extends MyStream[Nothing]

case class MyCons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

object MyStream {
  def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    MyCons(() => head, () => tail)
  }

  def empty[A]: MyStream[A] = MyEmpty

  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

object MainMyStream extends App {
  def expensive(x: Int) = {
    println("***** Doing the expensive operation")
    2 * x
  }

  val x: MyStream[Int] = MyCons(() => expensive(2), () => MyEmpty)
  x.headOption
  x.headOption

  val y = MyStream(1, 2, 3)
  println(y)
  println(y.toList)
  println(y.take(-2))
  println(y.take(0))
  println(y.take(2))
  println(y.take(22))
}
