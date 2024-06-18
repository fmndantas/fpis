import com.github.fmndantas.eight.Falsified
import com.github.fmndantas.eight.Gen
import com.github.fmndantas.eight.Passed
import com.github.fmndantas.eight.Prop
import com.github.fmndantas.eight.Result
import com.github.fmndantas.six.RNG
import com.github.fmndantas.six.SimpleRNG
import Utils.stairRNG

class PropSpec extends MultipleCases {
  val rng = SimpleRNG(42)

  test("forAll approves predicate against Gen samples") {
    val p = Prop.forAll(Gen.choose(100, 200)) { v =>
      v >= 100 && v < 200
    }
    val r = p.run(100, rng)
    assertEquals(r, Passed)
  }

  cases("forAll disapproves predicate against Gen samples")(
    (Gen.unit(10), (v: Int) => v != 10, Falsified("10", 0), rng),
    (Gen.choose(10, 20), (v: Int) => v <= 15, Falsified("16", 6), stairRNG(10))
  ) { case (gen, predicate, ans: Result, rng: RNG) =>
    val p = Prop.forAll(gen)(predicate)
    assertEquals(p.run(10, rng), ans)
  }

  cases("forAll disapproves predicate against Gen samples")(
    (Gen.boolean, (v: Boolean) => !v, Falsified("true", 1), stairRNG(0))
  ) { case (gen, predicate, ans: Result, rng: RNG) =>
    val p = Prop.forAll(gen)(predicate)
    assertEquals(p.run(10, rng), ans)
  }
}
