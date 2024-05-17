package com.github.fmndantas.five

sealed trait Stream[+A] {
  def headOption[A]: Option[A] = this match {
    case Empty         => None
    case Cons[A](h, t) => Some(h())
  }

  def toList[A]: List[A] = this match {
    case Empty         => List.empty[A]
    case Cons[A](h, t) => List(h()) ++ t().toList
  }

  def take(n: Int): Seq[A] =
    @annotation.tailrec
    def f(i: Int, current: Stream[A], result: Seq[A]): Seq[A] =
      if (i == n) result
      else
        current match {
          case Empty         => result
          case Cons[A](h, t) => f(i + 1, t(), result :+ h())
        }
    f(0, this, Seq.empty[A])

  def drop(n: Int): Seq[A] =
    @annotation.tailrec
    def f(i: Int, current: Stream[A]): Seq[A] =
      current match {
        case Empty => List.empty[A]
        case Cons(h, t) => {
          if (i == n) current.toList
          else f(i + 1, t())
        }
      }
    f(0, this)
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def empty[A]: Stream[A] = Empty

  def cons[A](h: => A, t: => Stream[A]): Stream[A] =
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail*))
}
