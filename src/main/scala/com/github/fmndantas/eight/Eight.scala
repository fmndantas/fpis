package com.github.fmndantas.eight

import com.github.fmndantas.eight.forAll

object Eight extends App:
  val intList = Gen.listOf(Gen.choose(0, 100))
  val prop =
    forAll(intList)(ns => ns.reverse.reverse == ns) &&
      forAll(intList)(ns => ns.reverse.headOption == ns.lastOption)
