// Copyright 2019 Jacob Kesinger

package ContinuedFraction

import com.typesafe.scalalogging.LazyLogging

case class CSTATE(p0: Double, p1: Double, q0: Double, q1: Double, pos: Int) extends LazyLogging {
  def hasConverged(tol: Double = 1e-16): Boolean = {


    if (pos <= 1) {
      false
    } else {
      val r0 = p0 / q0
      val r1 = p1 / q1
      val diff = math.abs(r1 - r0)
      val rerr = diff / math.abs(r0)
      logger.debug(s"$r0 $r1 $diff => $rerr")

      rerr < tol
    }
  }
}
