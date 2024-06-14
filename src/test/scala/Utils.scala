import com.github.fmndantas.six.RNG

object Utils:
  def mockRand[A](desiredValue: A)(rng: RNG) = (desiredValue, rng)

  def mockRNG(a: Int): RNG =
    new RNG:
      def nextInt = (a, mockRNG(a))
