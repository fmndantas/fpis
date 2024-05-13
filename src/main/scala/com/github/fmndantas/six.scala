package com.github.fmndantas

trait Six {
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
    val (a, rngA) = rng.nextInt
    (if (a < 0) -(a + 1) else a, rngA)
  }

  def double: Rand[Double] = map(nonNegativeInt)(_.toDouble / Int.MaxValue)

  // NOTE: S => (A, S)
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(f.andThen(unit))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      flatMap(rb) { b =>
        unit(f(a, b))
      }
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft((x: RNG) => (List.empty[A], x)) { case (acc, rand) =>
      map2(acc, rand) { case (a, b) => a :+ b }
    }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rngA) = f(rng)
      g(a)(rngA)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + n - 1 - mod >= 0) unit(mod)
    else nonNegativeLessThan(n)
  }

  /*
   * Encapsula transições de estado de
   * maneira funcional; faz isso retornando
   * um novo estado atualizado que resultou
   * da atualização de um estado passado
   */
  case class State[S, +A](run: S => (A, S)):
    def map[B](f: A => B): State[S, B] = flatMap(f andThen State.unit)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State { s0 =>
        val (a, s1) = run(s0)
        f(a).run(s1)
      }

  object State:
    def unit[A, S](a: A) = State[S, A](s => (a, s))

    def map2[A, B, C, S](sa: State[S, A], sb: State[S, B])(
        f: (A, B) => C
    ): State[S, C] = for {
      a <- sa
      b <- sb
    } yield (f(a, b))

    def sequence[A, S](ss: List[State[S, A]]): State[S, List[A]] =
      ss.foldLeft(State.unit[List[A], S](List.empty[A])) { case (acc, s) =>
        acc.flatMap(l => s.map(a => l :+ a))
      }

  def getNextInt: State[RNG, Int] = State(_.nextInt)
}
