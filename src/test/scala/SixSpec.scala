import org.scalatest.funspec.AnyFunSpec

import com.github.fmndantas.Six

class sutSpec extends AnyFunSpec {
  object sut extends Six
  import sut.{State, SimpleRNG, RNG}

  case class SomeState(x: Int)

  def mockRand[A](desiredValue: A)(rng: sut.RNG) = (desiredValue, rng)

  describe("Non-generalized functions") {
    it("map2 joins two rands") {
      val rng = SimpleRNG(42)
      val (result1, _) = mockRand(42)(rng)
      val (result2, _) = mockRand(42)(rng)
      val (joinedResult, _) = sut.map2(mockRand(42), mockRand(42))(_ + _)(rng)
      assertResult(result1 + result2)(joinedResult)
    }

    it("sequence transforms list of transitions into a single transition") {
      val rands = List(mockRand(1), mockRand(2), mockRand(3))
      val transition = sut.sequence(rands)
      val rng = SimpleRNG(42)
      val (resultingList, resultingRng) = transition(rng)
      assertResult(List(1, 2, 3))(resultingList)
      rands.lastOption.foreach { lastRand =>
        assertResult(lastRand(rng)._2)(resultingRng)
      }
    }

    it("nonNegativeLessThan returns aleatory number below specific n") {
      val rng = SimpleRNG(42)
      val (result, _) = sut.nonNegativeLessThan(10)(rng)
      assert(result < 10)
    }
  }

  describe("State") {
    it("Lifts some value") {
      val initialState = SomeState(-1)
      val s1 = State.unit[Int, SomeState](10)
      val (a, b) = s1.run(initialState)
      assertResult(10)(a)
      assertResult(b)(initialState)
    }

    it("Maps one state into another") {
      val initialState = SomeState(0)
      val s1 = State[SomeState, Int](s => (0, s.copy(x = s.x + 1)))
      val s2 = s1.map(_ + 10)
      val (r, s3) = s2.run(initialState)
      assertResult(10)(r)
      assertResult(SomeState(1))(s3)
    }

    it("Flatmaps one state into another") {
      val initialState = SomeState(0)
      val s1 = State[SomeState, Int](s => (0, s.copy(x = s.x + 1)))
      val s2 = s1.flatMap(a =>
        State { s =>
          (a + 10, s.copy(x = s.x + 1))
        }
      )
      val (r, s3) = s2.run(initialState)
      assertResult(10)(r)
      assertResult(SomeState(2))(s3)
    }

    it("Transform list of states into one state that generate list as result") {
      val s = State.sequence[Int, RNG](
        List(State.unit(42), State.unit(43), State.unit(44))
      )
      val (r, _) = s.run(SimpleRNG(42))
      assertResult(List(42, 43, 44))(r)
    }
  }

  it("How to use State to generate random integers") {
    val simpleRNG = SimpleRNG(100)
    val s = for {
      a <- State.unit[Int, RNG](0)
      b <- sut.getNextInt
      c <- sut.getNextInt
    } yield (a, b, c)
    val ((a, b, c), _) = s.run(simpleRNG)
    // println((a, b, c))
    assert(a != b)
    assert(b != c)
    assert(c != a)
  }
}
