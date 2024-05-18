package com.github.fmndantas.five

import com.github.fmndantas.five.Stream.empty

sealed trait Stream[+A] {
  def headOption[A]: Option[A] = this match {
    case Empty         => None
    case Cons[A](h, t) => Some(h())
  }

  def toList[A]: List[A] = this match {
    case Empty         => List.empty[A]
    case Cons[A](h, t) => List(h()) ++ t().toList
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
        case Empty => empty
        case Cons(h, t) =>
          if (i == n) suffix
          else f(i + 1, t())
      }
    f(0, this)

  def takeWhile(p: A => Boolean): Stream[A] =
    @annotation.tailrec
    def f(prefix: Seq[A], suffix: => Stream[A]): Stream[A] =
      suffix match {
        case Empty => Stream(prefix*)
        case Cons(h, t) =>
          lazy val evaluated_h = h()
          if (!p(evaluated_h)) Stream(prefix*)
          else f(prefix :+ evaluated_h, t())
      }
    f(Seq.empty[A], this)
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
