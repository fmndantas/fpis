package com.github.fmndantas.six

// NOTE:
// Encapsula transições de estado de
// maneira funcional; faz isso retornando
// um novo estado atualizado que resultou
// da atualização de um estado passado
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
