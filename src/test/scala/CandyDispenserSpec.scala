import org.scalatest.funspec.AnyFunSpec
import com.github.fmndantas.CandyDispenser

class CandyDispenserSpec extends AnyFunSpec {
  object sut extends CandyDispenser
  import sut.{Machine, Answer}

  // FIX: ?
  // it("Creates answer from machine") {
  //   val machine = Machine(false, 123, 321)
  //   val result = machine.createAnswer
  //   assertResult(Answer(321, 123))(result)
  // }

  // FIX: replace by last rule
  // it("Won't unlock the machine if there is not any candy left") {
  //   val initial = Machine(locked = true, candies = 0, coins = 10)
  //   val (result, machine) = sut.insertCoin(1).run(initial)
  //   assertResult(Machine(true, 0, 11), "Machine")(machine)
  // }

  describe("Inserting a coin") {
    describe("Into a locked machine") {
      it("Will unlock it if there is some candy left") {
        val initial = Machine(locked = true, candies = 10, coins = 10)
        val (result, machine) = sut.insertCoin.run(initial)
        assertResult(Machine(false, 10, 11), "Machine")(machine)
      }
    }
    describe("Into a unlocked machine does nothing") {
      it("On a machine with candies") {
        val initial = Machine(locked = false, candies = 10, coins = 10)
        val (_, machine) = sut.insertCoin.run(initial)
        assertResult(initial, "Machine with candies")(machine)
      }

      it("On a machine without candies") {
        val initial = Machine(locked = false, candies = 0, coins = 10)
        val (_, machine) = sut.insertCoin.run(initial)
        assertResult(initial, "Machine without candies")(machine)
      }
    }
  }

  describe("Turning the knob") {
    it("Will do nothing on a locked machine") {
      val initial = Machine(locked = true, candies = 10, coins = 10)
      val (result, machine) = sut.turnKnob.run(initial)
      assertResult(initial)(machine)
    }

    it(
      "On a unlocked machine will cause machine to dispense candy and become locked"
    ) {
      val initial = Machine(locked = false, candies = 10, coins = 10)
      val (result, machine) = sut.turnKnob.run(initial)
      assertResult(Machine(true, 9, 10))(machine)
    }
  }

  describe("A machine that's out of candies ignores") {
    it("Any coin insertion") {
      val initial = Machine(locked = true, candies = 0, coins = 10)
      val (_, machine) = sut.insertCoin.run(initial)
      assertResult(initial)(machine)
    }

    it("Any knob turning") {
      val initial = Machine(locked = false, candies = 0, coins = 10)
      val (_, machine) = sut.turnKnob.run(initial)
      assertResult(initial)(machine)
    }
  }
}
