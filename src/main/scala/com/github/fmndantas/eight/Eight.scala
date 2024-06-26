package com.github.fmndantas.eight

import com.github.fmndantas.six.SimpleRNG

object Eight extends App:
  // FIX: incomplete chapter
  val intList = Gen.choose(0, 100).listOfN(Gen.choose(0, 100))
  val prop =
    Prop.forAll(intList)(ns => ns.reverse.reverse == ns) &&
      Prop.forAll(intList)(ns => ns.reverse.headOption == ns.lastOption)
  val r = prop.run(100, SimpleRNG(System.currentTimeMillis))
  println(r)
