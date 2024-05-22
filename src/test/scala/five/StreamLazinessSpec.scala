import com.github.fmndantas.five.Stream

class StreamLazinessSpec extends munit.FunSuite {
  test("headOption is lazy") {
    Stream
      .cons[Int](
        1,
        Stream
          .cons({ assert(false, "headOption is not lazy"); 2 }, Stream.empty)
      )
      .headOption
  }

  test("take is lazy") {
    Stream
      .cons[Int](
        1,
        Stream.cons({ assert(false, "take is not lazy"); 2 }, Stream.empty)
      )
      .take(1)
  }

  test("drop is lazy") {
    Stream
      .cons[Int]({ assert(false, "drop is not lazy"); 1 }, Stream.empty)
      .drop(1)
  }

  test("takeWhile is lazy") {
    Stream
      .cons[Int](
        -2,
        Stream
          .cons({ assert(false, "takeWhile is not lazy"); -1 }, Stream.empty)
      )
      .takeWhile(_ > 0)
  }

  test("forAll is lazy") {
    Stream
      .cons[Int](
        -1,
        Stream.cons({ assert(false, "forAll is not lazy"); -1 }, Stream.empty)
      )
      .forAll(_ > 0)
  }

  test("map is lazy") {
    Stream
      .cons[Int](
        -1,
        Stream.cons({ assert(false, "map is not lazy"); -1 }, Stream.empty)
      )
      .map(_ + 1)
  }

  test("filter is lazy if and only if the first element satisfies predicate") {
    Stream
      .cons[Int](
        1,
        Stream.cons({ assert(false, "filter is not lazy"); -1 }, Stream.empty)
      )
      .filter(_ > 0)
  }

  test("flatMap is lazy") {
    Stream
      .cons[Int](1, { assert(false, "flatMap is not lazy"); Stream.empty })
      .flatMap(v => Stream(v, v))
  }

  test("zipWith is lazy") {
    val s1 = Stream.empty[Int]
    val s2 = Stream.cons({assert(false, "zipWith is not lazy"); 1}, Stream.empty[Int])
    s1.zipWith(s2)
  }
}
