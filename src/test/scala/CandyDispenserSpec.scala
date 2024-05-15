import org.scalatest.funspec.AnyFunSpec
import com.github.fmndantas.six.candyDispenser.CandyDispenser
import com.github.fmndantas.six.candyDispenser.Machine
import com.github.fmndantas.six.candyDispenser.Answer
import com.github.fmndantas.six.State

class CandyDispenserSpec extends AnyFunSpec {
  object sut extends CandyDispenser
  import sut.{Coin, Turn}

  describe(
    "Accepts sequence of inputs and return final state based on some initial state"
  ) {
    Seq(
      (2, 1, Seq(), Answer(coins = 2, candies = 1)),
      (0, 1, Seq(Coin, Turn), Answer(coins = 1, candies = 0)),
      (2, 0, Seq(Coin, Turn), Answer(coins = 2, candies = 0)),
      (2, 1, Seq(Coin, Turn), Answer(coins = 3, candies = 0)),
      (2, 1, Seq(Coin, Coin), Answer(coins = 3, candies = 1)),
      (10, 5, Seq.fill(4)(Seq(Coin, Turn)).flatten, Answer(coins = 14, candies = 1)),
    ).zipWithIndex.foreach { case (testData, kase) =>
      it(s"Case #${kase + 1}") {
        val (initialCoins, initialCandies, inputs, expectedAnswer) = testData
        val transitions = sut.simulate(inputs)
        // NOTE: machine is initially locked
        val (answer, _) =
          transitions.run(Machine(true, initialCandies, initialCoins))
        assertResult(expectedAnswer.coins, "coins")(answer.coins)
        assertResult(expectedAnswer.candies, "candies")(answer.candies)
      }
    }
  }
}
