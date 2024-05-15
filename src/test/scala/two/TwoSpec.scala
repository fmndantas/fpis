import org.scalatest.funspec.AnyFunSpec
import com.github.fmndantas.two.Two

class TwoSpec extends AnyFunSpec {
  object sut extends Two

  it("Returns fibonacci numbers") {
    val results = (0 to 9).map(sut.fib)
    assertResult(Seq(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))(results)
  }
}
