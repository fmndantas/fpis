import munit.Location

trait MultipleTests extends munit.FunSuite:
  def cases[A](description: String, cs: A*)(testCase: A => Unit)(using
      loc: munit.Location
  ) =
    cs.foreach(c =>
      test(description) {
        testCase(c)
      }
    )
