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
    val (rf, _) = g.sample.run(Utils.mockRNG(0))
    val (rt, _) = g.sample.run(Utils.mockRNG(1))
    assertEquals(rf, false)
    assertEquals(rt, true)
  }

  test("listOfN returns list of values using a generator") {
    val n = 1000
    val g = Gen.listOfN(n, Gen.choose(10, 20))
    val (r, _) = g.sample.run(rng)
    assertEquals(r.size, n)
    r.foreach(v => assert(10 <= v && v < 20, s"v = $v"))
  }

  test("tuple of ints") {
    val g1 =
      Gen.listOfN(2, Gen.choose(1000, 2000)).map { case a :: b :: _ => (a, b) }
    val ((a, b), _) = g1.sample.run(rng)
    println((a, b))
    assert(1000 <= a && a < 2000)
    assert(1000 <= b && b < 2000)
  }
}
