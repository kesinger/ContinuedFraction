package ContinuedFraction

import com.typesafe.scalalogging.LazyLogging

import scala.annotation.tailrec


// TODO: Make eval run resolvents until converge
// TODO: Figure out how to support sigs like a(x,n) and b(z1,z2,n), etc
// b(0) + K_{m-1}^\infty(a(m)/b(m))
case class CFRAC(a: Int => Double, b: Int => Double, state: Option[CSTATE] = None) extends  LazyLogging {
  // Modified from a StackOverflow answer
  private def repit[A](f: A => A, n: Int): A => A = {
    val aid: A => A = identity[A]
    Stream.iterate(aid)(_.andThen(f)).apply(n)
  }

  def advance(): CFRAC = {
    state match {
      case None =>
        val newstate = CSTATE(1, b(0), 0, 1, 1)
        CFRAC(a, b, Some(newstate))
      case Some(s) =>
        val k = s.pos
        val pk = b(k - 1) * s.p1 + a(k - 1) * s.p0
        val qk = b(k - 1) * s.q1 + a(k - 1) * s.q0
        val newstate = CSTATE(s.p1, pk, s.q1, qk, k + 1)
        CFRAC(a, b, Some(newstate))
    }
  }

  def advance(k: Int): CFRAC = {
    repit((x: CFRAC) => x.advance(), k)(this)
  }

  def eval(): Double = {
    val res = resolvents(100)
    res.last._1 / res.last._2
  }

  def hasConverged(tol: Double = 1e-16): Boolean = {
    state match {
      case None => false
      case Some(s) =>
        if (s.pos <= 1) {
          false
        } else {
          val r0 = s.p0 / s.q0
          val r1 = s.p1 / s.q1
          val diff = math.abs(r1 - r0)
          val rerr = diff / math.abs(r0)
          println(s"$r0 $r1 $diff => $rerr")
          rerr < tol
        }
    }
  }

  def backwardsRecurrence(k: Int) : Double = {
    @tailrec
    def impl(cur: Int, last: Double) : Double = {
      cur match {
        case _ if cur <= 0 => last
        case _ =>
          val next = a(cur)/(b(cur) + last)
          impl(cur-1, next)
      }
    }
    b(0) + impl(k, 0.0)
  }

  def resolvents(k: Int): Seq[(Double, Double)] = {
    @tailrec
    def impl(cur: Int, stop: Int, state: CSTATE, acc: Seq[(Double, Double)]): Seq[(Double, Double)] = {
      if (state.hasConverged()) {
        acc
      } else {
        cur match {
          case _ if cur < 0 => Seq()
          case _ if cur >= stop => acc
          case _ if cur == 0 =>
            impl(1, stop, CSTATE(1, b(0), 0, 1, 1), Seq((b(0), 1.0)))
          case _ if cur < stop =>
            val up = (state.p1, state.q1)
            // I've got concerns about the stability of this recursion
            val pk = b(cur - 1) * state.p1 + a(cur - 1) * state.p0
            val qk = b(cur - 1) * state.q1 + a(cur - 1) * state.q0
            val newstate = CSTATE(state.p1, pk, state.q1, qk, cur + 1)
            impl(cur + 1, stop, newstate, acc :+ up)
        }
      }
    }

    impl(2, k, CSTATE(1, b(0), 0, 1, 1), Seq())
  }

}

