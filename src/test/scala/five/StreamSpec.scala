import com.github.fmndantas.five.Stream

class StreamSpec extends munit.FunSuite with MultipleCases {
  cases("headOption optionally returns the Stream's first element")(
    (Stream(), None),
    (Stream(1), Some(1)),
    (Stream("foo", "bar"), Some("foo"))
  ) { case (stream, expectedResult) =>
    assertEquals(stream.headOption, expectedResult)
  }

  cases("toList converts stream to List")(
    (Stream.empty[Int], List.empty[Int]),
    (Stream(1, 2, 3, 4, 5), List(1, 2, 3, 4, 5)),
    (Stream("foo", "bar"), List("foo", "bar"))
  ) { case (stream, expectedList) =>
    assertEquals(stream.toList, expectedList)
  }

  cases("take(n) returns the first n elements of a Stream")(
    (Stream.empty[Int], 10, Seq.empty[Int]),
    // NOTE: n < stream.size
    (Stream(1, 2, 3, 4), 0, Seq.empty[Int]),
    (Stream(1, 2, 3, 4), 1, Seq(1)),
    (Stream(1, 2, 3, 4), 2, Seq(1, 2)),
    (Stream(1, 2, 3, 4), 3, Seq(1, 2, 3)),
    // NOTE: n == stream.size
    (Stream(1, 2, 3, 4), 4, Seq(1, 2, 3, 4)),
    // NOTE: n > stream.size
    (Stream(1, 2, 3, 4), 5, Seq(1, 2, 3, 4)),
    (Stream(1, 2, 3, 4), 6, Seq(1, 2, 3, 4))
  ) { case (stream, n, expectedSeq) =>
    assertEquals[Seq[Int], Seq[Int]](stream.take(n), expectedSeq)
  }

  cases("drop(n) skips the first n elements of a Stream")(
    (Stream.empty[Int], 10, Seq.empty[Int]),
    (Stream(1, 2, 3, 4), 0, Seq(1, 2, 3, 4)),
    (Stream(1, 2, 3, 4), 1, Seq(2, 3, 4)),
    (Stream(1, 2, 3, 4), 2, Seq(3, 4)),
    (Stream(1, 2, 3, 4), 3, Seq(4)),
    (Stream(1, 2, 3, 4), 4, Seq.empty[Int]),
    (Stream(1, 2, 3, 4), 5, Seq.empty[Int]),
    (Stream(1, 2, 3, 4), 6, Seq.empty[Int]),
  ) { case (stream, n, expectedSeq) =>
    assertEquals[Seq[Int], Seq[Int]](stream.drop(n), expectedSeq)
  }
}
