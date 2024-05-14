import org.scalatest.funspec.AnyFunSpec
import com.github.fmndantas.CandyDispenser

class CandyDispenserSpec extends AnyFunSpec {
  object sut extends CandyDispenser
  import sut.{Machine, Answer}
  
  it("Creates answer from machine") {
    val machine = Machine(false, 123, 321)
    val result = machine.createAnswer
    assertResult(Answer(321, 123))(result)
  }

  describe(
    "Inserting a coin into a locked machine will cause it to unlock if there's any candy left"
  ) {
    it("There is some candy left") {
      val initial = Machine(locked = true, candies = 10, coins = 10)
      val (result, machine) = sut.insertCoin(1).run(initial)
      assertResult(Machine(false, 10, 11), "Machine")(machine)
    }

    it("There is not some candy left") {
      val initial = Machine(locked = true, candies = 0, coins = 10)
      val (result, machine) = sut.insertCoin(1).run(initial)
      assertResult(Machine(true, 0, 11), "Machine")(machine)
    }
  }

  describe("Turning the knob") {
    it("Will do nothing in a locked machine") {
      val initial = Machine(locked = true, candies = 10, coins = 10)
      val (result, machine) = sut.turnKnob.run(initial)
      assertResult(initial)(machine)
    }

    it("Will cause machine to dispense candy and become locked") {
      val initial = Machine(locked = false, candies = 10, coins = 10)
      val (result, machine) = sut.turnKnob.run(initial)
      assertResult(Machine(true, 9, 10))(machine)
    }
  }
}
