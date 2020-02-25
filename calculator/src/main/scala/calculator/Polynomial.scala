package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Var(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {


    Var({
      val aValue = a()
      val bValue = b()
      val deltaValue = delta()

      if (deltaValue < 0) {
        Set()
      } else if (delta() == 0) {
        {
          Set(-b() / 2 * aValue)
        }
      } else {
        {
          Set(
            (-bValue + Math.sqrt(deltaValue)) / (2 * aValue),
            (-bValue - Math.sqrt(deltaValue)) / (2 * aValue)
          )
        }
      }
    })
  }
}
