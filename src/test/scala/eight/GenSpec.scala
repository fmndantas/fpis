import com.github.fmndantas.eight.Gen
import com.github.fmndantas.six.SimpleRNG

class GenSpec extends munit.FunSuite {
  val rng = SimpleRNG(42)

  test("unit returns a fixed value") {
    val g = Gen.unit(10)
    val (r, _) = g.sample.run(rng)
    assertEquals(r, 10)
  }

  test("boolean returns true or false") {
    val g = Gen.boolean
    val (rf, _) = g.sample.run(Utils.fixedRNG(0))
    val (rt, _) = g.sample.run(Utils.fixedRNG(1))
    assertEquals(rf, false)
    assertEquals(rt, true)
  }

  test("listOfNV1 returns list of values using a generator") {
    val n = 1000
    val g = Gen.listOfNV0(n, Gen.choose(10, 20))
    val (r, _) = g.sample.run(rng)
    assertEquals(r.size, n)
    r.foreach(v => assert(10 <= v && v < 20, s"v = $v"))
  }

  test("tuple of ints") {
    val g1 =
      Gen.listOfNV0(2, Gen.choose(1000, 2000)).map { case a :: b :: _ => (a, b) }
    val ((a, b), _) = g1.sample.run(rng)
    assert(1000 <= a && a < 2000)
    assert(1000 <= b && b < 2000)
  }

  test("flatMap") {
    val g = Gen
      .unit(10)
      .flatMap(a =>
        Gen
          .unit(a + 20)
          .flatMap(b => Gen.listOfNV0(5, Gen.unit(b)))
      )
    val (b, _) = g.sample.run(rng)
    assertEquals(b, List.fill(5)(30))
  }

  test("listOfN creates list of random values using int generator as size source") {
    val g = Gen.choose(1, 100).listOfN(Gen.choose(45, 55))
    val (a, _) = g.sample.run(rng)
    assert(a.size >= 45)
    assert(a.size < 55)
  }

  test("union combine two generators, pulling from both with equal likelihood") {
    val g0 = Gen.unit(1)
    val g1 = Gen.unit(2)
    val g2 = Gen.union(g0, g1).listOfN(Gen.unit(10))
    val (a, _) = g2.sample.run(rng)
    assert(a.exists(_ == 1))
    assert(a.exists(_ == 2))
  }
}
