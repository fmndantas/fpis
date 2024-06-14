package com.github.fmndantas

package object eight {
  def forAll[A](g: Gen[A])(p: A => Boolean): Boolean = false

  def putNBetweenLowerAndUpperIfNIsLessThanLower(
      n: Int,
      lower: Int,
      upperExclusive: Int
  ) =
    val dA = upperExclusive - lower
    val dB = lower - n
    val d = dB % dA
    if d == 0 then lower else upperExclusive - d
}

