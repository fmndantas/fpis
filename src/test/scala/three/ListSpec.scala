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
}
