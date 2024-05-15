import com.github.fmndantas.six.RNG
import com.github.fmndantas.six.SimpleRNG
import com.github.fmndantas.six.State
import org.scalatest.funspec.AnyFunSpec

class StateSpec extends AnyFunSpec {
  case class SomeState(x: Int)

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
