package com.github.fmndantas.two

trait Two {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def f(a: Int, b: Int, i: Int): Int = {
      if (i == n) a
      else f(b, a + b, i + 1)
    }
    f(0, 1, 0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    @annotation.tailrec
    def f(i: Int): Boolean =
      if (i == as.size - 1) true
      else if (!ordered(as(i), as(i + 1))) false
      else f(i + 1)
    if (as.size < 2) true
    else f(0)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)
}
