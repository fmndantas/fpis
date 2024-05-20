import com.github.fmndantas.five.Stream

class StreamLazinessSpec extends munit.FunSuite {
  test("headOption is not lazy") {
    Stream
      .cons[Int](
        1,
        Stream.cons({ assert(false, "headOption is not lazy"); 2 }, Stream.empty)
      )
      .headOption
  }

  test("take is not lazy") {
    Stream
      .cons[Int](
        1,
        Stream.cons({ assert(false, "take is not lazy"); 2 }, Stream.empty)
      )
      .take(1)
  }

  test("drop is not lazy") {
    Stream.cons[Int]({ assert(false, "drop is not lazy"); 1 }, Stream.empty).drop(1)
  }

  test("takeWhile is not lazy") {
    Stream
      .cons[Int](
        -2,
        Stream.cons({ assert(false, "takeWhile is not lazy"); -1 }, Stream.empty)
      )
      .takeWhile(_ > 0)
  }

  test("forAll is not lazy") {
    Stream
      .cons[Int](
        -1,
        Stream.cons({ assert(false, "forAll is not lazy"); -1 }, Stream.empty)
      )
      .forAll(_ > 0)
  }

  test("map is not lazy") {
    Stream
      .cons[Int](
        -1,
        Stream.cons({ assert(false, "map is not lazy"); -1 }, Stream.empty)
      )
      .map(_ + 1)
  }

  test("filter is not lazy") {
    Stream
      .cons[Int](
        -1,
        Stream.cons({ assert(false, "filter is not lazy"); -1 }, Stream.empty)
      )
      .filter(_ > 0)
  }
}
