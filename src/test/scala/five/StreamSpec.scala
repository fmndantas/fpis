import com.github.fmndantas.five.Stream

class StreamSpec extends munit.FunSuite with MultipleCases {
  cases("headOption optionally returns the Stream's first element")(
    (Stream(), None),
    (Stream(1), Some(1)),
    (Stream("foo", "bar"), Some("foo"))
  ) { case (stream, ans) =>
    assertEquals(stream.headOption, ans)
  }

  cases("toList converts stream to List")(
    (Stream.empty[Int], List.empty[Int]),
    (Stream(1, 2, 3, 4, 5), List(1, 2, 3, 4, 5)),
    (Stream("foo", "bar"), List("foo", "bar"))
  ) { case (stream, ans) =>
    assertEquals(stream.toList, ans)
  }

  cases("take(n) returns the first n elements of a Stream")(
    (Stream.empty[Int], 10, List.empty[Int]),
    // NOTE: n < stream.size
    (Stream(1, 2, 3, 4), 0, List.empty[Int]),
    (Stream(1, 2, 3, 4), 1, List(1)),
    (Stream(1, 2, 3, 4), 2, List(1, 2)),
    (Stream(1, 2, 3, 4), 3, List(1, 2, 3)),
    // NOTE: n == stream.size
    (Stream(1, 2, 3, 4), 4, List(1, 2, 3, 4)),
    // NOTE: n > stream.size
    (Stream(1, 2, 3, 4), 5, List(1, 2, 3, 4)),
    (Stream(1, 2, 3, 4), 6, List(1, 2, 3, 4))
  ) { case (stream, n, ans) =>
    assertEquals(stream.take(n).toList, ans)
  }

  cases("drop(n) skips the first n elements of a Stream")(
    (Stream.empty[Int], 10, List.empty[Int]),
    (Stream(1, 2, 3, 4), 0, List(1, 2, 3, 4)),
    (Stream(1, 2, 3, 4), 1, List(2, 3, 4)),
    (Stream(1, 2, 3, 4), 2, List(3, 4)),
    (Stream(1, 2, 3, 4), 3, List(4)),
    (Stream(1, 2, 3, 4), 4, List.empty[Int]),
    (Stream(1, 2, 3, 4), 5, List.empty[Int]),
    (Stream(1, 2, 3, 4), 6, List.empty[Int])
  ) { case (stream, n, ans) =>
    assertEquals(stream.drop(n).toList, ans)
  }

  cases(
    "takeWhile returns starting elements of a stream that match a predicate"
  )(
    (Stream.empty[Int], (x: Int) => x > 0, List.empty[Int]),
    (Stream(1, 1, 1, -1, -1, -1), (x: Int) => x > 0, Seq(1, 1, 1)),
    (Stream(-1, -1, -1, 1, 1, 1), (x: Int) => x > 0, Seq.empty[Int]),
    (Stream(1, 1, 1, 1, 1, 1), (x: Int) => x > 0, Seq.fill(6)(1)),
    (Stream(1, 1, 1, 1, 1, 2), (x: Int) => x != 2, Seq.fill(5)(1))
  ) { case (stream: Stream[Int], f: (Int => Boolean), ans: List[Int]) =>
    assertEquals(stream.takeWhile(f).toList, ans)
  }

  cases("forAll checks that all elements in a stream match a given predicate")(
    (Stream(1, 3, 5), (x: Int) => x % 2 != 0, true),
    (Stream(2, 4, 6), (x: Int) => x % 2 != 0, false)
  ) { case (stream: Stream[Int], f: (Int => Boolean), ans: Boolean) =>
    assertEquals(stream.forAll(f), ans)
  }

  // TEST: manual assert
  test("take laziness causes nothing to be printed") {
    Stream
      .cons[Int](
        1,
        Stream.cons({ println("take is not lazy"); 2 }, Stream.empty)
      )
      .take(1)
  }

  // TEST: manual assert
  test("drop laziness causes nothing to be printed") {
    Stream.cons[Int]({ println("drop is not lazy"); 1 }, Stream.empty).drop(1)
  }

  // TEST: manual assert
  test("takeWhile laziness causes nothing to be printed") {
    Stream
      .cons[Int](
        -1,
        Stream.cons({ println("takeWhile is not lazy"); -1 }, Stream.empty)
      )
      .takeWhile(_ > 0)
  }

  // TEST: manual assert
  test("forAll laziness causes nothing to be printed") {
    Stream
      .cons[Int](
        -1,
        Stream.cons({ println("forAll is not lazy"); -1 }, Stream.empty)
      )
      .forAll(_ > 0)
  }
}
