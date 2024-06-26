package com.github.fmndantas.seven

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Executors
import java.util.concurrent.Callable
import scala.util.Try
import scala.util.Success
import scala.util.Failure

object Blocking extends App {
  case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val fa = a(es)
      val fb = b(es)
      unit(f(fa.get, fb.get))(es)
    }

  // NOTE: map with timeout
  def map2b[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es =>
      new Future[C] {
        def cancel(evenIfRunning: Boolean) = ???

        def get(): C = get(Long.MaxValue, TimeUnit.SECONDS)

        def get(timeout: Long, timeUnit: TimeUnit): C =
          val fa = a(es)
          val fb = b(es)
          val timeoutNanos = TimeUnit.NANOSECONDS.convert(timeout, timeUnit)
          val (ra, t) = measureElapsedTime {
            fa.get(timeoutNanos, TimeUnit.NANOSECONDS)
          }
          val rb = fb.get(timeoutNanos - t, TimeUnit.NANOSECONDS)
          f(ra, rb)

        def isCancelled(): Boolean = ???

        def isDone(): Boolean = ???
      }

  def measureElapsedTime[A](thunk: => A): (A, Long) =
    val inicio = System.nanoTime
    val resultado = thunk
    val fim = System.nanoTime
    (resultado, fim - inicio)

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      // println(s"1, $es")
      es.submit(new Callable[A] {
        // println(s"2, $es")
        def call = {
          // println(s"3, $es")
          val r = a(es).get
          // println(s"4, r = $r")
          r
        }
      })

  def sequence[A](ps: Seq[Par[A]]): Par[Seq[A]] =
    ps.foldLeft(unit(Seq.empty))(map2b(_, _)(_ :+ _))

  val es = Executors.newFixedThreadPool(5)

  {
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
    println(f.get(10, TimeUnit.SECONDS))
  }

  {
    def makeFuture = es.submit(new Callable[Int] {
      def call = 42
    })
    val pars: List[Par[Int]] =
      (1 to 10).map(_ => (es: ExecutorService) => makeFuture).toList
    val par = sequence(pars)
    val f = run(es)(par)
    println(f.get)
  }

  {
    def g(a: Int) = fork(es => UnitFuture(a + 1))

    // FIX: use this
    // def g(a: Int) = fork(unit(a + 1))

    val p = g(10)
    val f = p(es)
    val r = Try {
      f.get(5, TimeUnit.SECONDS)
    }
    r match {
      case Success(v) => println(v)
      case Failure(e) =>
        e.printStackTrace()
    }
  }

  {
    def count[A](xs: Seq[A], f: (A => Int)): Par[Int] =
      val g = (a: A) => fork(_ => UnitFuture((f(a))))
      xs.map(g)
        .foldLeft[Par[Int]]((es: ExecutorService) => UnitFuture(0))(
          map2b(_, _)(_ + _)
        )

    def count2[A](xs: Seq[A], f: (A => Int)): Par[Int] =
      val g = (a: A) => fork(_ => UnitFuture((f(a))))
      map2b(sequence(xs.map(g)), _ => UnitFuture(()))((a, _) => a.sum)

    val paragraphs = Seq(
      "fernando matheus do nascimento dantas",
      "novo parágrafo",
      "foo bar zas",
      "ok ok ok ok ok ok ok ok ok ok ok"
    )
    def countWords(paragrafo: String) = paragrafo.split(" ").size

    val p = count2[String](paragraphs, countWords)
    val f = p(es)
    println(f.get)
  }

  {
    // NOTE: each fork occupies one thread
    // so, a deadlock will occur if n+1 simultaneous
    // forks are executed in a n thread-pool
    fork(fork(_ => UnitFuture(10)))(es).get(1, TimeUnit.SECONDS)
  }

  es.shutdown()
}
