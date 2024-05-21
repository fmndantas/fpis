import com.github.fmndantas.five.Stream

class OnesSpec extends munit.FunSuite {
  def ones: Stream[Int] = Stream.cons(1, ones)

  test(
    "Infinite streams supports expressions that terminates as soon the result is available"
  ) {
    assertEquals[Seq[Int], Seq[Int]](ones.take(5).toList, Seq.fill(5)(1))
    assertEquals[Seq[Int], Seq[Int]](
      ones.map(_ + 1).take(5).toList,
      Seq.fill(5)(2)
    )
    assertEquals(ones.forAll(_ != 1), false)
  }

  test(
    "Is it possible that some methodos in infinite streams never ends".ignore
  ) {
    assertEquals(ones.forAll(_ == 1), true)
  }
}
