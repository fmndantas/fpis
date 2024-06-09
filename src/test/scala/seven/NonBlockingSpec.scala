import java.util.concurrent.Executors
import com.github.fmndantas.seven.NonBlocking.*

class NonBlockingSpec extends munit.FunSuite {
  val es = Executors.newFixedThreadPool(5)

  test("unit") {
    val p = unit(10)
    val r = run(es)(p)
    assert(r.isSuccess)
    assertEquals(r.get, 10)
  }

  test("fork") {
    val p = fork(fork(fork(fork(fork(fork(fork(unit(10))))))))
    val r = run(es)(p)
    assert(r.isSuccess)
    assertEquals(r.get, 10)
  }
  
  test("large amount of forks") {
    def f(i: Int): Par[Int] =
      if i == 500000 then unit(42)
      else fork(f(i + 1))
    val r = run(es)(f(0))
    assert(r.isSuccess)
    assertEquals(r.get, 42)
  }

  test("map2") {
    val p0 = fork(fork(unit(10)))
    val p1 = fork(unit(10))
    val p2 = map2(p0, p1)(_ + _)
    val p3 = map2(p2, p0)(_ + _)
    val r = run(es)(p3)
    assert(r.isSuccess)
    assertEquals(r.get, 30)
  }

  test("sequence") {
    val n = 100
    val ps = List.range(0, n).map(unit)
    val r = run(es)(sequence(ps))
    assert(r.isSuccess)
    assertEquals(r.get, List.range(0, n))
  }

  test("parMap with List") {
    val n = 100000
    def somarUm(v: Int) = v + 1
    val p = parMap(List.range(0, n))(somarUm)
    val r = run(es)(p)
    assert(r.isSuccess)
    assertEquals(r.get, List.range(0, n).map(_ + 1))
  }

  test("parMap with Vector") {
    val n = 100000
    def somarUm(v: Int) = v + 1
    val p = parMap(Vector.range(0, n))(somarUm)
    val r = run(es)(p)
    assert(r.isSuccess)
    assertEquals(r.get, Vector.range(0, n).map(_ + 1))
  }

  test("run should not swallow exceptions") {
    val p = unitComErro(10)
    val r = run(es)(p)
    assert(r.isFailure)
  }

  test("run should not swallow exceptions") {
    val p0 = fork(fork(unitComErro(10)))
    val p1 = fork(unit(10))
    val p2 = map2(p0, p1)(_ + _)
    val p3 = map2(p2, p0)(_ + _)
    val r = run(es)(p3)
    assert(r.isFailure)
  }
}
