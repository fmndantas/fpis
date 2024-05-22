package com.github.fmndantas

package object five {
  case class IntegerState(value: Int):
    def increment =
      val incremented = this.copy(value = this.value + 1)
      Some(incremented.value, incremented)

    def keep = Some(this.value, this)

    def incrementUpToTen: Option[(Int, IntegerState)] =
      val incremented = this.copy(value = this.value + 1)
      if incremented.value > 10 then None
      else Some((incremented.value, incremented))

  case class FibonacciState(a: Int, b: Int):
    def next =
      val s1 = this.copy(a = b, b = a + b)
      Some(s1.a, s1)
}
