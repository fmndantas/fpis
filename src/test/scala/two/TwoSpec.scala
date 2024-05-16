import com.github.fmndantas.two.Two

class TwoSpec extends munit.FunSuite with MultipleCases {
  object sut extends Two

  test("Generates fibonacci numbers") {
    assertEquals[Seq[Int], Seq[Int]](
      (0 to 9).map(sut.fib),
      Seq(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    )
  }

  cases("Determines if integer array is sorted")(
    (Array.empty[Int], (a: Int, b: Int) => a <= b, true),
    (Array(999), (a: Int, b: Int) => a <= b, true),
    (Array(999, 1999), (a: Int, b: Int) => a > b, false),
    (Array(999, 1999), (a: Int, b: Int) => a <= b, true)
  ) { case (as, f, expectedIsSorted) =>
    assertEquals(sut.isSorted[Int](as, f), expectedIsSorted)
  }

  cases("Determines if string array is sorted")(
    (Array[String](), (a: String, b: String) => a.size <= b.size, true),
    (Array("f"), (a: String, b: String) => a.size <= b.size, true),
    (Array("f", "ff", "fff"), (a: String, b: String) => a.size <= b.size, true)
  ) { case (as, f, expectedIsSorted) =>
    assertEquals(sut.isSorted[String](as, f), expectedIsSorted)
  }

  test("Curries a function of two arguments") {
    val sum = (a: Int, b: Int) => a + b
    val curriedSum = sut.curry(sum)
    assertEquals(curriedSum(1)(2), 3)
  }

  test("Uncurries a function of two arguments") {
    val curriedProduct = (a: Int) => (b: Int) => a * b
    val product = sut.uncurry(curriedProduct)
    assertEquals(product(10, 10), 100)
  }

  test("An uncurried curried function is the same than the original unmodified function") {
    val g = (a: String, b: String) => a + b
    val f = sut.uncurry(sut.curry(g))
    assertEquals(f("foo", "bar"), "foobar")
  }
}
