import com.github.fmndantas.six.RNG
import com.github.fmndantas.six.SimpleRNG
import com.github.fmndantas.six.Six
import com.github.fmndantas.six.State

class SixSpec extends munit.FunSuite {
  object sut extends Six

  def mockRand[A](desiredValue: A)(rng: RNG) = (desiredValue, rng)

  test("map2 joins two rands") {
    val rng = SimpleRNG(42)
    val (result1, _) = mockRand(42)(rng)
    val (result2, _) = mockRand(42)(rng)
    val (joinedResult, _) = sut.map2(mockRand(42), mockRand(42))(_ + _)(rng)
    assertEquals(joinedResult, result1 + result2)
  }

  test("sequence transforms list of transitions into a single transition") {
    val rands = List(mockRand(1), mockRand(2), mockRand(3))
    val transition = sut.sequence(rands)
    val rng = SimpleRNG(42)
    val (resultingList, resultingRng) = transition(rng)
    assertEquals(resultingList, List(1, 2, 3))
    rands.lastOption.foreach { lastRand =>
      assertEquals(resultingRng, lastRand(rng)._2)
    }
  }

  test("nonNegativeLessThan returns aleatory number below specific n") {
    val rng = SimpleRNG(42)
    val (result, _) = sut.nonNegativeLessThan(10)(rng)
    assert(result < 10)
  }

  test("How to use State to generate random integers") {
    val simpleRNG = SimpleRNG(100)
    val s = for {
      a <- State.unit[Int, RNG](0)
      b <- sut.getNextInt
      c <- sut.getNextInt
    } yield (a, b, c)
    val ((a, b, c), _) = s.run(simpleRNG)
    assert(a != b)
    assert(b != c)
    assert(c != a)
  }
}
