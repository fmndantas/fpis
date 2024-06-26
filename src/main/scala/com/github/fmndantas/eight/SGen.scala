package com.github.fmndantas.eight

case class SGen[A](forSize: Int => Gen[A])

object SGen:
  def listOf[A](g: Gen[A]): SGen[List[A]] = 
    SGen(v => g.listOfN(Gen.unit(v)))
