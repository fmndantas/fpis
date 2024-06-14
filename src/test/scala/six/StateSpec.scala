import com.github.fmndantas.six.RNG
import com.github.fmndantas.six.SimpleRNG
import com.github.fmndantas.six.State

class StateSpec extends munit.FunSuite {
  case class SomeState(x: Int)

  test("Lifts some value") {
    val initialState = SomeState(-1)
    val s1 = State.unit[Int, SomeState](10)
    val (a, b) = s1.run(initialState)
    assertEquals(a, 10)
    assertEquals(b, initialState)
  }

  test("Maps one state into another") {
    val initialState = SomeState(0)
    val s1 = State[SomeState, Int](s => (0, s.copy(x = s.x + 1)))
    val s2 = s1.map(_ + 10)
    val (r, s3) = s2.run(initialState)
    assertEquals(r, 10)
    assertEquals(s3, SomeState(1))
  }

  test("Flatmaps one state into another") {
    val initialState = SomeState(0)
    val s1 = State[SomeState, Int](s => (0, s.copy(x = s.x + 1)))
    val s2 = s1.flatMap(a =>
      State { s =>
        (a + 10, s.copy(x = s.x + 1))
      }
    )
    val (r, s3) = s2.run(initialState)
    assertEquals(r, 10)
    assertEquals(s3, SomeState(2))
  }

  test("Transform list of states into one state that generate list as result") {
    val s = State.sequence[Int, RNG](
      List(State.unit(42), State.unit(43), State.unit(44))
    )
    val (r, v) = s.run(SimpleRNG(42))
    assertEquals(r, List(42, 43, 44))
    assertEquals(v, SimpleRNG(42))
  }
}
