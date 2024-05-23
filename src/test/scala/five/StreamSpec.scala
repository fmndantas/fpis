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
    assertEquals[Seq[Int], Seq[Int]](
      Stream.ones.take(20).toList,
      Seq.fill(20)(1)
    )
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

  cases(
    "zipWith zips streams while both streams have items"
  )(
    (Stream.empty[Int], Stream.empty[Int], Seq.empty[(Int, Int)]),
    (Stream(1, 2, 3), Stream(10, 11), Seq((1, 10), (2, 11))),
    (Stream(1, 2), Stream(10, 11, 12), Seq((1, 10), (2, 11))),
    (Stream(1, 2, 3), Stream(10, 11, 12), Seq((1, 10), (2, 11), (3, 12)))
  ) { case (streamA, streamB, ans) =>
    assertEquals[Seq[(Int, Int)], Seq[(Int, Int)]](
      streamA.zipWith(streamB).toList,
      ans
    )
  }

  cases(
    "zipAll zips streams using None if any stream ends early"
  )(
    (
      Stream.empty[Int],
      Stream.empty[Int],
      Seq.empty[(Option[Int], Option[Int])]
    ),
    (
      Stream(1, 2, 3),
      Stream(10, 11),
      Seq((Some(1), Some(10)), (Some(2), Some(11)), (Some(3), None))
    ),
    (
      Stream(1, 2),
      Stream(10, 11, 12),
      Seq((Some(1), Some(10)), (Some(2), Some(11)), (None, Some(12)))
    ),
    (
      Stream(1, 2, 3),
      Stream(10, 11, 12),
      Seq((Some(1), Some(10)), (Some(2), Some(11)), (Some(3), Some(12)))
    )
  ) { case (streamA, streamB, ans) =>
    assertEquals[Seq[(Option[Int], Option[Int])], Seq[
      (Option[Int], Option[Int])
    ]](streamA.zipAll(streamB).toList, ans)
  }

  cases(
    "startsWith determines if a stream has another stream as prefix"
  )(
    (Stream.empty[Int], Stream.empty[Int], true),
    (Stream(1, 2, 3, 4), Stream(1, 2, 3, 4), true),
    (Stream(1, 2, 3, 4), Stream(10, 2, 3, 4), false),
    (Stream(1, 2, 3, 4), Stream(1, 2, 3, 3), false),
    (Stream(1, 2, 3, 4), Stream(1, 2), true),
    (Stream(1, 2, 3, 4), Stream(1, 1), false),
    (Stream(1, 2, 3, 4), Stream.empty[Int], true),
    (Stream(1, 2), Stream(1, 2, 3, 4), false),
    (Stream.empty[Int], Stream(10), false),
    (Stream(10), Stream.empty[Int], true),
    (Stream.ones, Stream(1), true),
    (Stream.ones, Stream(2), false),
    (Stream("a", "b"), Stream("a"), true),
    (Stream("t", "b"), Stream("a"), false)
  ) { case (streamA, streamB, ans) =>
    assertEquals(streamA.startsWith(streamB), ans)
  }

  cases(
    "tails returns stream of stream suffixes"
  )(
    (Stream.empty[Int], List.empty[List[Int]]),
    (Stream(1), List(List(1))),
    (Stream(1, 2, 3), List(List(1, 2, 3), List(2, 3), List(3)))
  ) { case (stream, ans) =>
    assertEquals[List[List[Int]], List[List[Int]]](
      stream.tails.toList.map(_.toList),
      ans
    )
  }

  cases("scanRight Stream[Int] suite")(
    (Stream.empty[Int].scanRight(0)(_ + _), List(0)),
    (Stream(1, 2, 3).scanRight(0)(_ + _), List(6, 5, 3, 0)),
    (Stream(1, 2, 3).scanRight(0)((a, b) => b - a), List(-6, -5, -3, 0)),
    (Stream(1, 2, 3).scanRight(1)(_ * _), List(6, 6, 3, 1))
  ) { case (stream: Stream[Int], ans) =>
    assertEquals[List[Int], List[Int]](stream.toList, ans)
  }

  cases("scanRight Stream[Stream[Int]] suite")(
    (
      Stream(1, 2).scanRight(Stream.empty[Int])((a, b) => Stream.cons(a, b)),
      List(List(1, 2), List(2), List.empty[Int])
    ),
    (
      Stream(1, 2, 3).scanRight(Stream(0))((a, b) => Stream.cons(a, b)),
      List(List(1, 2, 3, 0), List(2, 3, 0), List(3, 0), List(0))
    )
  ) { case (stream, ans) =>
    assertEquals[List[List[Int]], List[List[Int]]](
      stream.toList.map(_.toList),
      ans
    )
  }

  cases("scanRight Stream[String] suite")(
    (
      Stream("t", "s", "e", "t").scanRight("")(_ + _),
      List("tset", "set", "et", "t", "")
    ),
    (
      Stream("t", "s", "e", "t").scanRight("")((a, b) => b + a),
      List("test", "tes", "te", "t", "")
    )
  ) { case (stream, ans) =>
    assertEquals[List[String], List[String]](stream.toList, ans)
  }
}
