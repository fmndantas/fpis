package com.github.fmndantas.seven

import java.util.concurrent.Callable
import java.util.concurrent.CountDownLatch
import java.util.concurrent.ExecutorService
import java.util.concurrent.atomic.AtomicReference
import scala.util.Try

object NonBlocking {
  sealed trait Future[A] {
    private[seven] def apply(cb: Try[A] => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  private type Callback[A] = Try[A] => Unit

  def run[A](es: ExecutorService)(p: Par[A]): Try[A] =
    val ref = new AtomicReference[Try[A]]
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
        def apply(cb: Callback[A]): Unit = cb(Try(a))
      }

  def unitWithError[A](a: A): Par[A] =
    es =>
      new Future[A] {
        def apply(cb: Callback[A]): Unit =
          cb(Try(throw new RuntimeException("Proposital failure")))
      }

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      new Future[A] {
        def apply(cb: Callback[A]): Unit =
          eval(es)(a(es)(cb))
      }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      def call = r
    })

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es =>
      new Future[C] {
        def apply(cb: Callback[C]): Unit =
          var ra: Option[Try[A]] = None
          var rb: Option[Try[B]] = None
          val sync = Actor[Either[Try[A], Try[B]]](es) {
            case Left(ta) =>
              rb match
                case None => ra = Some(ta)
                case Some(tb) =>
                  val t = for {
                    va <- ta
                    vb <- tb
                  } yield f(va, vb)
                  eval(es)(cb(t))
            case Right(tb) =>
              ra match
                case None => rb = Some(tb)
                case Some(ta) =>
                  val t = for {
                    va <- ta
                    vb <- tb
                  } yield f(va, vb)
                  eval(es)(cb(t))
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

  // NOTE: slow because :+ (appended) is O(n) amortized
  // def sequence[A](ps: Seq[Par[A]]): Par[Seq[A]] =
  //   ps.foldLeft(unit(Seq.empty))((a, b) => fork(map2(a, b)(_ :+ _)))

  // NOTE: fast because +: (prepended) is O(1)
  def sequence[A](ps: Seq[Par[A]]): Par[Seq[A]] =
    ps.foldRight(unit(Seq.empty))((a, b) => fork(map2(a, b)(_ +: _)))

  def parMap[A, B](ys: Seq[A])(f: A => B): Par[Seq[B]] =
    val asyncF = (a: A) => fork(unit(f(a)))
    sequence(ys.map(asyncF))
}
