package eight

import com.github.fmndantas.eight.Gen
import com.github.fmndantas.eight.SGen
import com.github.fmndantas.six.SimpleRNG

class SGenSpec extends munit.FunSuite {
  val rng = SimpleRNG(42)

  test("listOf produces a list generator") {
    val g0 = Gen.unit(42)
    val s = SGen.listOf(g0)
    val g1 = s.forSize(10) 
    val (r, _) = g1.sample.run(rng)
    assertEquals(r, List.fill(10)(42))
  }
}
