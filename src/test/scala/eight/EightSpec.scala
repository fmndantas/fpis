import com.github.fmndantas.eight.putNBetweenLowerAndUpperIfNIsLessThanLower

class EightSpec extends MultipleCases {
  cases("keepNBetweenLowerAndUpperIfNIsLessThanLower")(
    (150, 200, 149, 199),
    (150, 200, 148, 198),
    (150, 200, 100, 150),
    (150, 200, 99, 199),
    (100, 101, 99, 100),
    (100, 101, 100, 100),
    (2, 10, 0, 8),
    (1, 2, 0, 1),
    (10, 20, 1, 11),
    (5, 10, 4, 9)
  ) { case (lower, upper, n, ans) =>
    assertEquals(putNBetweenLowerAndUpperIfNIsLessThanLower(n, lower, upper), ans)
  }
}
