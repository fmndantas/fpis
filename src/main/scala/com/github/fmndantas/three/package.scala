package com.github.fmndantas

package object three {
  def flip[A, B](f: (A, B) => B): (B, A) => B = (b, a) => f(a, b)
}
