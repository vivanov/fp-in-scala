package ru.lester.fpinscala.ch6

import ru.lester.fpinscala.ch6.State._

/**
 * User: vvivanov
 * Date: 01.12.2014
 * Time: 20:45
 */

trait RNG {
  def nextInt: (Int, RNG)
}
case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  def Simple(seed: Long): RNG = SimpleRNG(seed)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
  	val (newVal, newRng) = rng.nextInt
  	(if(newVal < 0) -(newVal + 1) else newVal, newRng) 
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r)  = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
  	val (i, r1) = rng.nextInt
  	val (d, r2) = double(r1)
  	((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
  	val ((i, d), r) = intDouble(rng)
  	((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)	
  }

  def ints(count: Int) (rng: RNG): (List[Int], RNG) = {
  	def go(l: List[Int], r: RNG, cnt: Int): (List[Int], RNG) = {
      if(cnt == 0) (l, r)
      else {
        val (i, r1) = r.nextInt 
        go(i :: l, r1, cnt - 1)      	
      }
  	}
    go(Nil, rng, count)
  }

  type Rand[+A] = RNG => (A, RNG)
  //type Rand[+A] = State[RNG, A]
  //type Rand[+A] = State[RNG, A]

  def int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A]) (f: A => B): Rand[B] = {
  	rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
  	}
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def doubleM: Rand[Double] = map(nonNegativeInt)(_ / Int.MaxValue.toDouble + 1 )

  def map2[A, B, C](ra: Rand[A], rb: Rand[B]) (f: (A, B) => C): Rand[C] = {
  	rng => {
  		val (a, rng1) = ra(rng)
  		val (b, rng2) = rb(rng1)
  		(f(a, b), rng2)
  	}
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
  	map2(ra, rb) ((_, _))
  }

  def intDoubleM2: Rand[(Int, Double)] = both(int, double)

  def doubleIntM2: Rand[(Double, Int)] = both(double, int)

  def sequenceMy[A](fs: List[Rand[A]]): Rand[List[A]] = {
  	rng => {
  		fs.foldRight((Nil: List[A], rng)) { case (ra, (l, r)) =>  val (a, rr) = ra(r); (a :: l, rr) }	
  	}
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
  	fs.foldRight(unit(List[A]())) ((f, acc) => map2(f, acc)(_ :: _))
  }

  def intsS(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
    	val (a, r1) = f(rng)
    	val (b, r2) = g(a)(r1)
    	(b, r2)
    }  
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
    	if(i + (n - 1) - mod >= 0) unit(mod)
    	else nonNegativeLessThan(n) 
    } )	
  }

  def nonNegativeBetween(start: Int, endExclusive: Int): Rand[Int] = {
    flatMap(nonNegativeLessThan(endExclusive))(i => {
      if(i >= start) unit(i) else nonNegativeLessThan(endExclusive)
    })
  }

  def mapf[A, B](r: Rand[A])(f: A => B): Rand[B] = {
  	flatMap(r)(a => unit(f(a)))
  }

  def map2f[A, B, C](ra: Rand[A], rb: Rand[B]) (f: (A, B) => C): Rand[C] = {
  	flatMap(ra){a => mapf(rb){b => f(a, b)}}
  }

  type RandS[+A] = State[RNG, A]


  def nonNegativeIntS: RandS[Int] = for {
    rng <- get[RNG]
    (newVal, newRng) = rng.nextInt
    _ <- set[RNG](newRng)
  } yield if(newVal < 0) - (newVal + 1) else newVal

  def nonNegativeLessThanS(n: Int): RandS[Int] =
    nonNegativeIntS.flatMap(i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) State.unit(mod)
      else nonNegativeLessThanS(n)
    })

  def nonNegativeBetweenS(start: Int, endExclusive: Int): RandS[Int] = {
    nonNegativeLessThanS(endExclusive).flatMap(i => {
      if(i >= start) State.unit(i) else nonNegativeLessThanS(endExclusive)
    })
  }
}