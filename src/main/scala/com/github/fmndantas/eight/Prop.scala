package com.github.fmndantas.eight

import com.github.fmndantas.six.RNG
import com.github.fmndantas.five.Stream

type AmountOfTestCases = Int
type FailedCase = String
type SuccessCount = Int

sealed trait Result:
  def isFalsified: Boolean

case object Passed extends Result:
  def isFalsified: Boolean = false

case class Falsified(failedCase: FailedCase, amountOfSuccesses: SuccessCount)
    extends Result:
  def isFalsified: Boolean = true

case class Prop(run: (AmountOfTestCases, RNG) => Result)

object Prop:
  def forAll[A](g: Gen[A])(p: A => Boolean): Prop = Prop { (n, rng) =>
    Stream
      .unfold(rng)(s => Some(g.sample.run(s)))
      .zipWith(Stream.from(0))
      .take(n)
      .map[Result] { (a, i) =>
        if p(a) then Passed else Falsified(a.toString, i)
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }
