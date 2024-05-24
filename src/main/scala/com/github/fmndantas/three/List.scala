package com.github.fmndantas.three

import com.github.fmndantas.three.flip

sealed trait List[+A] {
  @annotation.tailrec
  final def foldLeft[B](z: B)(f: (B, A) => B): B =
    this match
      case Cons(h, t) => t.foldLeft(f(z, h))(f)
      case _          => z

  def reverse: List[A] =
    foldLeft(List.empty[A])(flip(List.cons))

  def foldRight[B](z: B)(f: (A, B) => B): B =
    this.reverse.foldLeft(z)(flip(f))

  // NOTE: based on foldRight
  def append[A2 >: A](v: A2): List[A2] = foldRight(List(v))(List.cons)

  def appendAll[A2 >: A](v: List[A2]): List[A2] = foldRight(v)(List.cons)
}
case object Nil extends List[Nothing]
case class Cons[+A](h: A, t: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail*))

  def empty[A]: List[A] = Nil

  def cons[A](h: A, t: List[A]): List[A] = Cons(h, t)

  def concatenate[A](v: List[List[A]]): List[A] =
    v.foldLeft(List.empty[A])((b, a) => b.appendAll(a))
}
