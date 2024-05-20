import com.github.fmndantas.five.Stream

class StreamLazinessSpec extends munit.FunSuite {
  // TEST: manual assert, not good
  test("headOption laziness causes nothing to be printed") {
    Stream
      .cons[Int](
        1,
        Stream.cons({ println("headOption is not lazy"); 2 }, Stream.empty)
      )
      .headOption
  }

  // TEST: manual assert, not good
  test("take laziness causes nothing to be printed") {
    Stream
      .cons[Int](
        1,
        Stream.cons({ println("take is not lazy"); 2 }, Stream.empty)
      )
      .take(1)
  }

  // TEST: manual assert, not good
  test("drop laziness causes nothing to be printed") {
    Stream.cons[Int]({ println("drop is not lazy"); 1 }, Stream.empty).drop(1)
  }

  // TEST: manual assert, not good
  test("takeWhile laziness causes nothing to be printed") {
    Stream
      .cons[Int](
        -2,
        Stream.cons({ println("takeWhile is not lazy"); -1 }, Stream.empty)
      )
      .takeWhile(_ > 0)
  }

  // TEST: manual assert, not good
  test("forAll laziness causes nothing to be printed") {
    Stream
      .cons[Int](
        -1,
        Stream.cons({ println("forAll is not lazy"); -1 }, Stream.empty)
      )
      .forAll(_ > 0)
  }

  // TEST: manual assert, not good
  test("map laziness causes nothing to be printed") {
    Stream
      .cons[Int](
        -1,
        Stream.cons({ println("map is not lazy"); -1 }, Stream.empty)
      )
      .map(_ + 1)
  }
}
