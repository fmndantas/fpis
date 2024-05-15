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
}
