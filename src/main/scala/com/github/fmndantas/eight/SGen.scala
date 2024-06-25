package com.github.fmndantas.eight

case class SGen[A](forSize: Int => Gen[A])
