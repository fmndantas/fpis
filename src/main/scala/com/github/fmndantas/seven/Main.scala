package com.github.fmndantas.seven

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Executors
import java.util.concurrent.Callable

object Seven extends App {
  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  def unit[A](a: A): Par[A] =
    es => UnitFuture(a)

  case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val fa = a(es)
      val fb = b(es)
      unit(f(fa.get, fb.get))(es)
    }

  def map2b[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es =>
      new Future[C] {
        def cancel(evenIfRunning: Boolean) = ???

        def get(): C = ???

        def get(timeout: Long, timeUnit: TimeUnit): C =
          val fa = a(es)
          val fb = b(es)
          val timeoutNanos = TimeUnit.NANOSECONDS.convert(timeout, timeUnit)
          val (ra, t) = medirTempoTranscorrido {
            fa.get(timeoutNanos, TimeUnit.NANOSECONDS)
          }
          val rb = fb.get(timeoutNanos - t, TimeUnit.NANOSECONDS)
          f(ra, rb)

        def isCancelled(): Boolean = ???

        def isDone(): Boolean = ???
      }

  def medirTempoTranscorrido[A](thunk: => A): (A, Long) =
    val inicio = System.nanoTime
    val resultado = thunk
    val fim = System.nanoTime
    (resultado, fim - inicio)

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(new Callable[A] {
        def call = a(es).get
      })

  val es = Executors.newFixedThreadPool(10)

  val f1 = es.submit(new Callable[Int] {
    def call =
      Thread.sleep(2000)
      15
  })
  val f2 = es.submit(new Callable[Int] {
    def call =
      Thread.sleep(2000)
      10
  })
  val p1: Par[Int] = es => f1
  val p2: Par[Int] = es => f2
  val p = map2b(p1, p2)(_ + _)
  val f = run(es)(p)
  val r = f.get(0, TimeUnit.MILLISECONDS)
  println(r)
}
