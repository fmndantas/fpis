import com.github.fmndantas.five.IntegerState
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
  ) { case (stream, f, ans) =>
    assertEquals[Seq[Int], Seq[Int]](stream.takeWhile(f).toList, ans)
  }

  cases("forAll checks that all elements in a stream match a given predicate")(
    (Stream(1, 3, 5), (x: Int) => x % 2 != 0, true),
    (Stream(2, 4, 6), (x: Int) => x % 2 != 0, false)
  ) { case (stream: Stream[Int], f: (Int => Boolean), ans: Boolean) =>
    assertEquals(stream.forAll(f), ans)
  }

  cases("map modifies stream by applying function to all elements")(
    (Stream.empty[Int], (x: Int) => x + 1, Seq.empty[Int]),
    (Stream(1, 1, 1), (x: Int) => x + 1, Seq(2, 2, 2)),
    (Stream(-1, -1, -1), (x: Int) => -x, Seq(1, 1, 1))
  ) { case (stream, f, ans) =>
    assertEquals[Seq[Int], Seq[Int]](stream.map(f).toList, ans)
  }

  cases("filter keep stream items that conforms to predicate")(
    (Stream.empty[Int], (x: Int) => x >= 0, Seq.empty[Int]),
    (Stream(-1, 0, 1), (x: Int) => x >= 0, Seq(0, 1)),
    (Stream(-1, 0, 1), (x: Int) => x < 0, Seq(-1))
  ) { case (stream, f, ans) =>
    assertEquals[Seq[Int], Seq[Int]](stream.filter(f).toList, ans)
  }

  cases("flatMap creates stream of streams")(
    (Stream(1, 2), (x: Int) => Stream.empty[Int], Seq.empty[Int]),
    (Stream(1, 2), (x: Int) => Stream(x + 1, x + 1), Seq(2, 2, 3, 3))
  ) { case (stream, f: (Int => Stream[Int]), ans) =>
    assertEquals[Seq[Int], Seq[Int]](stream.flatMap(f).toList, ans)
  }

  test("ones generates infinite stream filled with ones") {
    assertEquals[Seq[Int], Seq[Int]](Stream.ones.take(20).toList, Seq.fill(20)(1))
  }

  test("from generates infinite stream like (n, n+1, n+2, ...)") {
    assertEquals[Seq[Int], Seq[Int]](
      Stream.from(0).take(10).toList,
      (0 until 10).toList
    )
  }

  test("fibs generates the infinite stream of Fibonacci numbers") {
    assertEquals[Seq[Int], Seq[Int]](
      Stream.fibs.take(10).toList,
      List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    )
  }

  cases(
    "unfold creates streams taking an initial state and creating the next states"
  )(
    (
      Stream.unfold[Int, IntegerState](IntegerState(0))(_.increment),
      10,
      (1 to 10).toList
    ),
    (
      Stream.unfold[Int, IntegerState](IntegerState(1))(_.keep),
      10,
      List.fill(10)(1)
    ),
    (
      Stream.unfold[Int, IntegerState](IntegerState(4))(_.incrementUpToTen),
      10,
      (5 to 10).toList
    )
  ) { case (stream, amountOfItens, ans) =>
    assertEquals[List[Int], List[Int]](
      stream.take(amountOfItens).toList,
      ans,
      s"Ans should be $ans"
    )
  }

  test("constant creates infinite stream with a constant integer") {
    assertEquals(Stream.constant(42).take(10).toList, List.fill(10)(42))
  }
}
