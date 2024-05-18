package com.github.fmndantas.five

sealed trait Stream[+A] {
  // NOTE: v1
  // def headOption: Option[A] = this match {
  //   case Cons[A](h, t) => Some(h())
  //   case _             => None
  // }

  // NOTE: v2
  def headOption: Option[A] = foldRight(None)((a, b) => Some(a))

  def toList: List[A] = this match {
    case Cons[A](h, t) => List(h()) ++ t().toList
    case _             => List.empty[A]
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  def take(n: Int): Stream[A] =
    @annotation.tailrec
    def f(i: Int, prefix: Seq[A], suffix: => Stream[A]): Stream[A] =
      if (i == n) Stream(prefix*)
      else
        suffix match {
          case Empty      => Stream(prefix*)
          case Cons(h, t) => f(i + 1, prefix :+ h(), t())
        }
    f(0, Seq.empty[A], this)

  def drop(n: Int): Stream[A] =
    @annotation.tailrec
    def f(i: Int, suffix: => Stream[A]): Stream[A] =
      suffix match {
        case Empty => Stream.empty
        case Cons(h, t) =>
          if (i == n) suffix
          else f(i + 1, t())
      }
    f(0, this)

  // NOTE: v1
  // def takeWhile(p: A => Boolean): Stream[A] =
  //   @annotation.tailrec
  //   def f(prefix: Seq[A], suffix: => Stream[A]): Stream[A] =
  //     suffix match {
  //       case Cons(h, t) =>
  //         lazy val evaluated_h = h()
  //         if (!p(evaluated_h)) Stream(prefix*)
  //         else f(prefix :+ evaluated_h, t())
  //       case _ => Stream(prefix*)
  //     }
  //   f(Seq.empty[A], this)

  // NOTE: v2
  // NOTE: why "{ case (a, b) }" evaluates b?
  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty)((a, b) =>
      if !p(a) then Stream.empty else Stream.cons(a, b)
    )

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)
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
