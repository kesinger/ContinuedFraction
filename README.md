Just an extremely minimal continued fraction class.


Evaluates

    CFRAC(a,b) = 
      b0  + a1 
            -------
            b1 + a2
                 --------
                 b2 + ....
                 
                 

USAGE:

    val a = (n:Int) => 1.0
    val b = (n:Int) => n

    val cf : CFRAC = CFRAC(a,b)
    val res : Double = cf.eval()

Or

    val a = (x: Double, n: Int) => 2.0*x / n
    val b = (x:Double, n: Int) => 1.0
    val cfx = ContinuedFraction(a,b)
    val res0 = cfx(0)
    val res1 = cfx(1)
    

One can go from ContinuedFraction to CFRAC as

    val frac = cfx.curry(-1)
    // Same as:
    val af = (n:Int)=>a(-1,n)
    val bf = (n:Int)=>b(-1,n)
    val frac = CFRAC(af,bf)


The fraction is evaluated via the 
*fundamental recurrence formulas* 
(https://en.wikipedia.org/wiki/Generalized_continued_fraction)
with some attempts at automatically detecting convergence.


Also implemented is backwards recursion:

    // Computes 10th approximant at x=4
    val bacrec = cfx.curry(4).backwardsRecurrence(10)
   

and resolvents:

    // ( (P0,Q0), (P1,Q1), .... , (P49,Q49) )
    val reso = cfx.curry(math.sqrt(2)).resolvents(50)