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

case class Prop(run: (AmountOfTestCases, RNG) => Result):
  def &&(other: Prop): Prop = Prop { (n, rng) =>
    val left = this.run(n, rng)
    left match
      case Passed =>
        other.run(n, rng) match
          case Passed          => Passed
          case Falsified(f, a) => Falsified(s"Right prop failed: $f", a)
      case Falsified(f, a) => Falsified(s"Left prop failed: $f", a)
  }

  def ||(other: Prop): Prop = Prop { (n, rng) =>
    val left = this.run(n, rng)
    val right = other.run(n, rng)
    (left, right) match
      case (Falsified(fl, al), Falsified(fr, ar)) => Falsified(s"Left failed with: $fl; Right failed with: $fr", al + ar)
      case _                                      => Passed
  }

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
