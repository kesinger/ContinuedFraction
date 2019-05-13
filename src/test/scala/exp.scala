// Copyright Jacob Kesinger 2019
package ContinuedFraction

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FlatSpec, Matchers}


class exp extends FlatSpec with Matchers with LazyLogging {
  def tester(x:Double, delta: Double=>Double, tol:Double=2e-15) = {
    math.abs(delta(x)) < tol
  }
  // Eq (11.1.1) Cuyt et al
  "cf.exp quad" should "be math.exp" in {
    val bq = (x:Double, n:Int) => {
      n match{
        case 1 => 2-x
        case _ => 1.0
      }
    }
    val aq: (Double, Int) => Double = (x:Double, n:Int) => {
      n match {
        case 0 => Double.NaN
        case 1 => 2*x
        case 2 => x*x/6
        case _ => x*x/ (4.0*(2*n-3)*(2*n-1))
      }
    }
    val cfq: ContinuedFraction = ContinuedFraction(aq, bq)
    val delta: Double => Double = (x: Double) => cfq(x) - math.exp(x)

  }

  // Eq (11.1.3) Cuyt et al
  "cf.exp" should "be math.exp" in {
    val b = (_: Int) => 1.0
    val a = (m: Int) => {
      m match {
        case 1 => 1.0
        case _ if (m % 2) == 1 =>
          1.0 / (2 * m)
        case _ if (m % 2) == 0 =>
          -1.0 / (2 * (m - 1))
      }
    }
    val nsteps = 20
    val cfexp: Double => Double = (x: Double) => CFRAC((n: Int) => a(n) * x, b).eval()
    val delta: Double => Double = (x: Double) => cfexp(x) - math.exp(x)

    assert(tester(1, delta))
    assert(tester(0, delta))
    assert(tester(-1, delta))
    // This seems rather large
    assert(tester(10, delta, 2e-8))
  }

  "contfrac.exp" should "be math.exp" in {
    val b = (_: Double, _: Int) => 1.0
    val a = (x: Double, m: Int) => {
      val am = m match {
        case 1 => 1.0
        case _ if (m % 2) == 1 =>
          1.0 / (2 * m)
        case _ if (m % 2) == 0 =>
          -1.0 / (2 * (m - 1))
      }
      x * am
    }
    val cf: ContinuedFraction = ContinuedFraction(a, b)
    val delta: Double => Double = (x: Double) => cf(x) - math.exp(x)
    assert(tester(1, delta))
    assert(tester(0, delta))
    assert(tester(-1, delta))
    assert(tester(10, delta, 2e-8))
  }

}
