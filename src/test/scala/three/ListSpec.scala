import com.github.fmndantas.three.List

class ListSpec extends MultipleCases {
  cases("foldLeft")(
    (List(1, 2, 3).foldLeft(0)(_ + _), 6),
    (List(1, 2, 3).foldLeft(0)(_ - _), -6),
    (List(1, 2, 3).foldLeft(1)(_ * _), 6)
  ) { case (real, expected) =>
    assertEquals[Int, Int](real, expected)
  }

  cases("foldLeft")(
    (List(1, 2, 3, 4).foldLeft(List.empty[Int])((b, _) => b), List.empty[Int])
  ) { case (real, expected) =>
    assertEquals[List[Int], List[Int]](real, expected)
  }

  test("foldLeft") {
    assertEquals(List(10.0, 5.0).foldLeft(2.0)(_ / _), 0.04)
  }

  test("reverse") {
    assertEquals(List(1, 2, 3, 4, 5).reverse, List(5, 4, 3, 2, 1))
  }

  test("foldRight") {
    assertEquals(List("t", "e", "s", "t").foldRight("")(_ + _), "test")
  }

  cases("foldRight")(
    (List(10.0, 5.0).foldRight(2.0)(_ / _), 4.0),
    (List(10.0, 5.0).foldRight(2.0)((a, b) => b / a), 0.04)
  ) { case (result, ans) =>
    assertEquals(result, ans)
  }

  test("append") {
    assertEquals(List(1, 2, 3).append(4), List(1, 2, 3, 4))
  }
}
