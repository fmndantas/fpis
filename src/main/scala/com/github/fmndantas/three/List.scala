package com.github.fmndantas.three

sealed trait List[+A] {
  @annotation.tailrec
  final def foldLeft[B](z: B)(f: (B, A) => B): B = 
    this match
      case Cons(h, t) => t.foldLeft(f(z, h))(f)
      case _ => z

  def reverse: List[A] =
    foldLeft(List.empty[A])((b, a) => List.cons(a, b))
}
case object Nil extends List[Nothing]
case class Cons[+A](h: A, t: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail*))

  def empty[A]: List[A] = Nil

  def cons[A](h: A, t: List[A]): List[A] = Cons(h, t)
}
