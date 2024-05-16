import com.github.fmndantas.two.Two

class TwoSpec extends munit.FunSuite with MultipleTests {
  object sut extends Two

  test("Generates fibonacci numbers") {
    assertEquals[Seq[Int], Seq[Int]](
      (0 to 9).map(sut.fib),
      Seq(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    )
  }

  cases(
    "Determines if integer array is sorted",
    (Array.empty[Int], (a: Int, b: Int) => a <= b, true),
    (Array(999), (a: Int, b: Int) => a <= b, true),
    (Array(999, 1999), (a: Int, b: Int) => a > b, false),
    (Array(999, 1999), (a: Int, b: Int) => a <= b, true)
  ) { case (as, f, expectedIsSorted) =>
    assertEquals(sut.isSorted[Int](as, f), expectedIsSorted)
  }

  cases(
    "Determines if string array is sorted",
    (Array[String](), (a: String, b: String) => a.size <= b.size, true),
    (Array("f"), (a: String, b: String) => a.size <= b.size, true),
    (Array("f", "ff", "fff"), (a: String, b: String) => a.size <= b.size, true),
  ) { case (as, f, expectedIsSorted) =>
    assertEquals(sut.isSorted[String](as, f), expectedIsSorted)
  }
}
