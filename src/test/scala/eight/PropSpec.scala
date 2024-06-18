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

  test("&& creates Prop that passes when both source Props passes") {
    val p0 = Prop.forAll(Gen.choose(10, 15))(_ < 15)
    // NOTE: p1 will fail
    val p1 = Prop.forAll(Gen.choose(10, 20))(_ < 15)
    val ok = p0 && p0
    val noByLeft = p1 && p0
    val noByRight = p0 && p1
    assertEquals(ok.run(10, rng), Passed, "ok")
    assertEquals(
      noByLeft.run(10, stairRNG(10)),
      Falsified("Left prop failed: 15", 5),
      "left"
    )
    assertEquals(
      noByRight.run(10, stairRNG(10)),
      Falsified("Right prop failed: 15", 5),
      "right"
    )
  }

  test("|| creates Prop that passes if any Prop passes") {
    val p0 = Prop.forAll(Gen.choose(10, 15))(_ < 15)
    // NOTE: p1 will fail
    val p1 = Prop.forAll(Gen.choose(10, 20))(_ < 15)
    val p2 = Prop.forAll(Gen.choose(10, 20))(_ < 11)
    val ok1 = p0 || p1
    val ok2 = p1 || p0
    val no = p1 || p2
    assertEquals(ok1.run(10, rng), Passed, "ok1")
    assertEquals(ok2.run(10, rng), Passed, "ok2")
    assertEquals(
      no.run(10, stairRNG(10)),
      Falsified("Left failed with: 15; Right failed with: 11", 5 + 1),
      "no"
    )
  }
}
