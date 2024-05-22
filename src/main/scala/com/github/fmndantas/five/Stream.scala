package com.github.fmndantas.five

import com.github.fmndantas.five.IntegerState
import com.github.fmndantas.five.Stream.unfold

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

  // NOTE: v1
  // def take(n: Int): Stream[A] =
  //   @annotation.tailrec
  //   def f(i: Int, prefix: Seq[A], suffix: => Stream[A]): Stream[A] =
  //     if (i == n) Stream(prefix*)
  //     else
  //       suffix match {
  //         case Cons(h, t) => f(i + 1, prefix :+ h(), t())
  //         case _          => Stream(prefix*)
  //       }
  //   f(0, Seq.empty[A], this)

  // NOTE: v2
  def take(n: Int): Stream[A] = unfold((0, this)) { s =>
    s match
      case (`n`, _)              => None
      case (counter, Cons(h, t)) => Some(h(), (counter + 1, t()))
      case _                     => None
  }

  def drop(n: Int): Stream[A] =
    @annotation.tailrec
    def f(i: Int, suffix: => Stream[A]): Stream[A] =
      suffix match {
        case Cons(h, t) =>
          if (i == n) suffix
          else f(i + 1, t())
        case _ => Stream.empty
      }
    f(0, this)

  // NOTE: v1
  // def takeWhile(p: A => Boolean): Stream[A] =
  //   @annotation.tailrec
  //   def f(prefix: Seq[A], suffix: => Stream[A]): Stream[A] =
  //     suffix match {
  //       case Cons(h, t) =>
  //         lazy val evaluatedH = h()
  //         if (!p(evaluatedH)) Stream(prefix*)
  //         else f(prefix :+ evaluatedH, t())
  //       case _ => Stream(prefix*)
  //     }
  //   f(Seq.empty[A], this)

  // NOTE: v2
  // NOTE: why "{ case (a, b) }" evaluates b?
  // def takeWhile(p: A => Boolean): Stream[A] =
  //   foldRight(Stream.empty)((a, b) =>
  //     if !p(a) then Stream.empty else Stream.cons(a, b)
  //   )

  // NOTE: v3
  def takeWhile(p: A => Boolean): Stream[A] =
    Stream.unfold(this) { s =>
      s match
        case Cons(h, t) if p(h()) => Some(h(), t())
        case _                    => None
    }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  // NOTE: v1
  // def map[B](f: A => B): Stream[B] =
  //   foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

  // NOTE: v2
  def map[B](f: A => B): Stream[B] =
    unfold(this) { s =>
      s match
        case Cons(h, t) => Some(f(h()), t())
        case _          => None
    }

  def filter(p: A => Boolean): Stream[A] = foldRight(Stream.empty) { (a, b) =>
    if p(a) then Stream.cons(a, b) else b
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => f(a).append(b))

  def append[A2 >: A](other: => Stream[A2]): Stream[A2] =
    foldRight(other)((a, b) => Stream.cons(a, b))

  def zipWith[B](other: Stream[B]): Stream[(A, B)] =
    Stream.unfold((this, other)) { case (s1, s2) =>
      (s1, s2) match
        case (Cons(h1, t1), Cons(h2, t2)) => Some(((h1(), h2()), (t1(), t2())))
        case _                            => None
    }
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

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match
      case Some((a, s)) => cons(a, unfold(s)(f))
      case _            => empty

  // NOTE: can be simplified? Yes. See v2 below
  // def fibs: Stream[Int] =
  //   def f(a: Int, b: Int): Stream[Int] =
  //     Stream.cons(a + b, f(b, a + b))
  //   Stream.cons(0, f(1, 0))

  // NOTE: v2
  def fibs: Stream[Int] = cons(0, unfold(FibonacciState(0, 1))(_.next))

  // NOTE: v1
  // def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // NOTE: v2
  def from(n: Int): Stream[Int] =
    cons(n, unfold(IntegerState(n))(_.increment))

  def ones: Stream[Int] = unfold(IntegerState(1))(_.keep)

  def constant(a: Int): Stream[Int] = unfold(IntegerState(a))(_.keep)
}
