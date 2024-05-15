import com.github.fmndantas.six.candyDispenser.Machine
import org.scalatest.funspec.AnyFunSpec

class MachineSpec extends AnyFunSpec {
  describe("Inserting a coin") {
    describe("Into a locked machine") {
      it("Will unlock it if there is some candy left") {
        val m0 = Machine(locked = true, candies = 10, coins = 10)
        val m1 = m0.insertCoin
        assertResult(Machine(false, 10, 11))(m1)
      }
    }
    describe("Into a unlocked machine does nothing") {
      it("On a machine that has some candies") {
        val m0 = Machine(locked = false, candies = 10, coins = 10)
        val m1 = m0.insertCoin
        assertResult(m0)(m1)
      }

      it("On a machine that's out of candies") {
        val m0 = Machine(locked = false, candies = 0, coins = 10)
        val m1 = m0.insertCoin
        assertResult(m0)(m1)
      }
    }
  }

  describe("Turning the knob") {
    it("Will do nothing on a locked machine") {
      val m0 = Machine(locked = true, candies = 10, coins = 10)
      val m1 = m0.turnKnob
      assertResult(m0)(m1)
    }

    it(
      "On a unlocked machine will cause machine to dispense candy and become locked"
    ) {
      val m0 = Machine(locked = false, candies = 10, coins = 10)
      val m1 = m0.turnKnob
      assertResult(Machine(true, 9, 10))(m1)
    }
  }

  describe("A machine that's out of candies ignores") {
    it("Any coin insertion") {
      val m0 = Machine(locked = true, candies = 0, coins = 10)
      val m1 = m0.insertCoin
      assertResult(m0)(m1)
    }

    it("Any knob turning") {
      val m0 = Machine(locked = false, candies = 0, coins = 10)
      val m1 = m0.turnKnob
      assertResult(m0)(m1)
    }
  }
}
