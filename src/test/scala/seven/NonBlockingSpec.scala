import java.util.concurrent.Executors
import com.github.fmndantas.seven.{NonBlocking => sut}
import com.github.fmndantas.seven.NonBlocking.Par
import com.github.fmndantas.seven.NonBlocking.Callback
import com.github.fmndantas.seven.NonBlocking.Future
import scala.util.Try
import scala.util.Success

class NonBlockingSpec extends munit.FunSuite {
  val es = Executors.newFixedThreadPool(1)

  test("unit") {
    val p = sut.unit(10)
    val r = sut.run(es)(p)
    assert(r.isSuccess)
    assertEquals(r.get, 10)
  }

  test("fork") {
    val p = sut.fork(
      sut.fork(sut.fork(sut.fork(sut.fork(sut.fork(sut.fork(sut.unit(10)))))))
    )
    val r = sut.run(es)(p)
    assert(r.isSuccess)
    assertEquals(r.get, 10)
  }

  test("fork stress test") {
    def f(i: Int): Par[Int] =
      if i == 500000 then sut.unit(42)
      else sut.fork(f(i + 1))
    val r = sut.run(es)(f(0))
    assert(r.isSuccess)
    assertEquals(r.get, 42)
  }

  test("map2") {
    val p0 = sut.fork(sut.fork(sut.unit(10)))
    val p1 = sut.fork(sut.unit(10))
    val p2 = sut.map2(p0, p1)(_ + _)
    val p3 = sut.map2(p2, p0)(_ + _)
    val r = sut.run(es)(p3)
    assert(r.isSuccess)
    assertEquals(r.get, 30)
  }

  test("sequence") {
    val n = 500000
    val ps = List.range(0, n).map(sut.unit)
    val r = sut.run(es)(sut.sequence(ps))
    assert(r.isSuccess)
    assertEquals(r.get, List.range(0, n))
  }

  test("parMap with List") {
    val n = 100000
    def sumOne(v: Int) = v + 1
    val p = sut.parMap(List.range(0, n))(sumOne)
    val r = sut.run(es)(p)
    assert(r.isSuccess)
    assertEquals(r.get, List.range(0, n).map(_ + 1))
  }

  test("parMap with Vector") {
    val n = 500000
    def sumOne(v: Int) = v + 1
    val p = sut.parMap(Vector.range(0, n))(sumOne)
    val r = sut.run(es)(p)
    assert(r.isSuccess)
    assertEquals(r.get, Vector.range(0, n).map(_ + 1))
  }

  test("run should not swallow exceptions") {
    val p = unitWithError(10)
    val r = sut.run(es)(p)
    assert(r.isFailure)
  }

  // NOTE: added just for error handling tests
  def unitWithError[A](a: A): Par[A] =
    es =>
      new Future[A] {
        def apply(cb: Callback[A]): Unit =
          cb(Try(throw new RuntimeException("Proposital failure")))
      }

  test("run should not swallow exceptions") {
    val p0 = sut.fork(sut.fork(unitWithError(10)))
    val p1 = sut.fork(sut.unit(10))
    val p2 = sut.map2(p0, p1)(_ + _)
    val p3 = sut.map2(p2, p0)(_ + _)
    val r = sut.run(es)(p3)
    assert(r.isFailure)
  }

  test("run should not swallow exceptions") {
    val n = 100
    val ps = List.range(0, n).map(sut.unit) :+ unitWithError(10)
    val r = sut.run(es)(sut.sequence(ps))
    assert(r.isFailure)
  }

  test(
    "choice should run the correct par based on outcome of conditional operation"
  ) {
    val pt = sut.choice[Int](sut.unit(true))(sut.unit(1), sut.unit(0))
    val pf = sut.choice[Int](sut.unit(false))(sut.unit(1), sut.unit(0))
    val pe = sut.choice[Int](sut.unit(true))(unitWithError(-1), sut.unit(0))
    assertEquals(sut.run(es)(pt).get, 1)
    assertEquals(sut.run(es)(pf).get, 0)
    assert(sut.run(es)(pe).isFailure)
  }

  test("choiceN should run the operation chosen by par") {
    val p = sut.choiceN(sut.unit(2))((0 to 10).map(sut.unit))
    val r = sut.run(es)(p)
    assert(r.isSuccess)
    val Success(n) = r
    assertEquals(n, 2)
  }

  test("flatMap stress test") {
    var pi = sut.unit(0)
    val n = 500000
    (1 to n).map(i => pi = sut.flatMap(pi)(_ => sut.unit(i)))
    assertEquals(sut.run(es)(pi).get, n)
  }
}
