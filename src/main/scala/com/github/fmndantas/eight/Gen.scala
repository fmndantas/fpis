package com.github.fmndantas.eight

import com.github.fmndantas.six.RNG
import com.github.fmndantas.six.State
import com.github.fmndantas.six.Six
import com.github.fmndantas.eight.putNBetweenLowerAndUpperIfNIsLessThanLower

case class Gen[A](sample: State[RNG, A]):
  def map[B](f: A => B): Gen[B] = Gen(this.sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(
    this.sample.flatMap(v => f(v).sample)
  )

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfNV0(n, this))

object six extends Six

object Gen:
  def choose[A](lower: Int, upperExclusive: Int): Gen[Int] =
    Gen(
      six
        .getNonNegativeIntLessThan(upperExclusive)
        .map { n =>
          if n >= lower then n
          else
            putNBetweenLowerAndUpperIfNIsLessThanLower(n, lower, upperExclusive)
        }
    )

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(
    six.getNonNegativeIntLessThan(2).map(Map(0 -> false, 1 -> true))
  )

  def listOfNV0[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))
