import com.github.fmndantas.six.candyDispenser.Machine

class MachineSpec extends munit.FunSuite {
  test(
    "Inserting a coin into a locked machine will unlock it if there is some candy left"
  ) {
    val m0 = Machine(locked = true, candies = 10, coins = 10)
    val m1 = m0.insertCoin
    assertEquals(m1, Machine(false, 10, 11))
  }

  test(
    "Inserting a coin into a unlocked machine does nothing on a machine that has some candies"
  ) {
    val m0 = Machine(locked = false, candies = 10, coins = 10)
    val m1 = m0.insertCoin
    assertEquals(m1, m0)
  }

  test(
    "Inserting a coin into a unlocked machine does nothing on a machine that's out of candies"
  ) {
    val m0 = Machine(locked = false, candies = 0, coins = 10)
    val m1 = m0.insertCoin
    assertEquals(m1, m0)
  }

  test("Turning the knob will do nothing on a locked machine") {
    val m0 = Machine(locked = true, candies = 10, coins = 10)
    val m1 = m0.turnKnob
    assertEquals(m1, m0)
  }

  test(
    "Turning the knob on a unlocked machine will cause machine to dispense candy and become locked"
  ) {
    val m0 = Machine(locked = false, candies = 10, coins = 10)
    val m1 = m0.turnKnob
    assertEquals(m1, Machine(true, 9, 10))
  }

  test("A machine that's out of candies ignores any coin insertion") {
      val m0 = Machine(locked = true, candies = 0, coins = 10)
      val m1 = m0.insertCoin
      assertEquals(m1, m0)
  }

  test("A machine that's out of candies ignores any knob turning") {
      val m0 = Machine(locked = false, candies = 0, coins = 10)
      val m1 = m0.turnKnob
      assertEquals(m1, m0)
  }
}
