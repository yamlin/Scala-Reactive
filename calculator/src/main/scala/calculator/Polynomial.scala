package calculator

import java.text.DecimalFormat

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      val valueB = b()
      valueB * valueB - 4 * a() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      if (delta() < 0) Set()
      else if (delta() == 0) Set(-b() / (2 * a()))
      else {
        val valueNegB = -b()
        val sqrt = Math.sqrt(delta())
        val doubleA = 2 * a()
        Set((valueNegB + sqrt) / doubleA, (valueNegB - sqrt) / doubleA)
      }
    }
  }
}
