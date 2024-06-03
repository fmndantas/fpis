package com.github.fmndantas.seven

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Executors
import java.util.concurrent.Callable
import java.util.concurrent.TimeoutException

object Seven extends App {
  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  def unit[A](a: A): Par[A] =
    es => UnitTimedFuture(a)

  // case class UnitFuture[A](get: A) extends Future[A] {
  //   def cancel(foo: Boolean) = false
  //   def get(timeout: Long, timeUnit: TimeUnit) = get
  //   def isCancelled(): Boolean = false
  //   def isDone(): Boolean = true
  // }

  class UnitTimedFuture[A](innerGet: => A) extends Future[A] {
    private lazy val lazyInnerGet = innerGet

    def cancel(foo: Boolean) = false

    def get() = lazyInnerGet

    def get(timeout: Long, timeUnit: TimeUnit) =
      val (a, t) = medirTempoTranscorrido(lazyInnerGet)
      if t > timeout then throw TimeoutException() else a

    def isCancelled(): Boolean = false

    def isDone(): Boolean = true
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val fa = a(es)
      val fb = b(es)
      unit(f(fa.get, fb.get))(es)
    }

  extension [A](f: Future[A])
    def temporizar(timeoutNanos: Long): FutureTemporizado[A] =
      FutureTemporizado(timeoutNanos, f)

  def medirTempoTranscorrido[A](thunk: => A): (A, Long) =
    val inicio = System.nanoTime
    val resultado = thunk
    val fim = System.nanoTime
    (resultado, fim - inicio)

  case class FutureTemporizado[A](
      timeoutNanossegundos: Long,
      future: Future[A]
  ) {
    def flatMap[B](f: (A, Long) => FutureTemporizado[B]): FutureTemporizado[B] =
      val (a, t) = medirTempoTranscorrido {
        future.get(timeoutNanossegundos, TimeUnit.NANOSECONDS)
      }
      f(a, timeoutNanossegundos - t)

    def map[B](f: (A, Long) => B): FutureTemporizado[B] =
      val (a, t) = medirTempoTranscorrido {
        future.get(timeoutNanossegundos, TimeUnit.NANOSECONDS)
      }
      val tempoRestante = timeoutNanossegundos - t
      val b = f(a, tempoRestante)
      UnitTimedFuture(b).temporizar(tempoRestante)

    def converterParaFuture: Future[A] = this._2
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C)(
      timeoutMilissegundos: Long = 1000
  ): Par[C] =
    es =>
      {
        for {
          (ra, tra) <- a(es).temporizar(1000 * timeoutMilissegundos)
          (rb, trb) <- b(es).temporizar(tra)
        } yield f(ra, rb)
      }.converterParaFuture

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(new Callable[A] {
        def call = a(es).get
      })

  val es = Executors.newFixedThreadPool(4)

  val p = map2(
    unit({ Thread.sleep(1000); 15 }),
    unit({ Thread.sleep(1000); 15 })
  )(_ + _)(1)
  val resultado = run(es)(p)
  require(resultado.get == 30)
}
