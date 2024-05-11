package com.github.fmndantas

package six {
  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRng) = rng.nextInt
    (if (n < 0) -(n + 1) else n, nextRng)
  }

  def double: Rand[Double] = map(nonNegativeInt)(_.toDouble / Int.MaxValue)

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (nextN, nextRng) = s(rng)
    (f(nextN), nextRng)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rngA) = ra(rng)
      val (b, rngB) = rb(rng)
      (f(a, b), rngB)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft((x: RNG) => (List.empty[A], x)) { case (acc, rand) =>
      map2(acc, rand) { case (a, b) => a :+ b }
    }
}
