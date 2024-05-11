import org.scalatest.funspec.AnyFunSpec

import com.github.fmndantas.six._

class SixSpec extends AnyFunSpec {
  def mockRand[A](desiredValue: A)(rng: RNG) = (desiredValue, rng)

  it("map2 joins two rands") {
    val rng = SimpleRNG(42)
    val (result1, _) = nonNegativeInt(rng)
    val (result2, _) = double(rng)
    val (joinedResult, _) = map2(nonNegativeInt, double)(_ + _)(rng)
    assertResult(result1 + result2)(joinedResult)
  }

  it("sequence transforms list of transitions into a single transition") {
    val rands = List(mockRand(1), mockRand(2), mockRand(3))
    val transition = sequence(rands)
    val rng = SimpleRNG(42)
    val (resultingList, resultingRng) = transition(rng)
    assertResult(List(1, 2, 3))(resultingList)
    rands.lastOption.foreach { lastRand =>
      assertResult(lastRand(rng)._2)(resultingRng)
    }
  }
}
