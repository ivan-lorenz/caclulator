package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var(scala.math.pow(b(), 2) - (4 * a() * c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Var(delta() match {
      case d if d < 0 => Set.empty[Double]
      case d if d == 0 => Set(-b() / (2 * a()))
      case _ =>
        val sqrtDelta = scala.math.sqrt(delta())
        Set.empty + (-b() + sqrtDelta) / (2 * a()) + (-b() - sqrtDelta) / (2 * a())
    })
  }
}
