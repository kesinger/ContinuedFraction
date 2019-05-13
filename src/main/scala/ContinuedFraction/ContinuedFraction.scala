// Copyright Jacob Kesinger 2019

package ContinuedFraction


case class ContinuedFraction(a: (Double, Int) => Double,
                             b: (Double, Int) => Double) {
  private val cfx = (x: Double) => CFRAC((n: Int) => a(x, n), (n: Int) => b(x, n))
  def apply(x:Double) : Double = cfx(x).eval()
  def curry(x:Double) : CFRAC = cfx(x)
}