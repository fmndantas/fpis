package com.github.fmndantas.seven

import java.util.concurrent.ExecutorService
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CountDownLatch
import java.util.concurrent.Callable

object NonBlocking {
  sealed trait Future[A] {
    private[seven] def apply(k: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A =
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { v =>
      ref.set(v)
      latch.countDown
    }
    latch.await()
    ref.get

  def unit[A](a: A): Par[A] =
    es =>
      new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      new Future[A] {
        def apply(cb: A => Unit): Unit =
          eval(es)(a(es)(cb))
      }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      def call = r
    })

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es =>
      new Future[C] {
        def apply(cb: C => Unit): Unit =
          var ra: Option[A] = None
          var rb: Option[B] = None
          val sync = Actor[Either[A, B]](es) {
            case Left(va) =>
              rb match
                case None     => ra = Some(va)
                case Some(vb) => eval(es)(cb(f(va, vb)))
            case Right(vb) =>
              ra match
                case None     => rb = Some(vb)
                case Some(va) => eval(es)(cb(f(va, vb)))
          }
          a(es)(va => sync ! Left(va))
          b(es)(vb => sync ! Right(vb))
      }

  // def sequence[A](ps: List[Par[A]]): Par[List[A]] =
  //   @annotation.tailrec
  //   def loop(prefix: Par[List[A]], suffix: List[Par[A]]): Par[List[A]] =
  //     if suffix.size == 0 then prefix
  //     else loop(fork(map2(prefix, suffix.head)(_ :+ _)), suffix.tail)
  //   loop(unit(List.empty), ps)
  
  // NOTE: lento porque :+ (appended) é O(n) amortizado
  // def sequence[A](ps: Seq[Par[A]]): Par[Seq[A]] =
  //   ps.foldLeft(unit(Seq.empty))((a, b) => fork(map2(a, b)(_ :+ _)))

  // NOTE: rápido porque +: (prepended) é O(1)
  def sequence[A](ps: Seq[Par[A]]): Par[Seq[A]] =
    ps.foldRight(unit(Seq.empty))((a, b) => fork(map2(a, b)(_ +: _)))

  def parMap[A, B](ys: Seq[A])(f: A => B): Par[Seq[B]] =
    val asyncF = (a: A) => fork(unit(f(a)))
    sequence(ys.map(asyncF))
}
