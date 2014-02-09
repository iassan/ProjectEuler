package org.bnb.jb.euler.common

import math._
import scala.collection.immutable.HashMap

/**
 * @author Jacek Bilski
 * @version $Revision: 174 $
 *          $Id: Numbers.scala 174 2013-08-20 12:32:38Z jacek.bilski $
 */
object Numbers {

	type Number = BigInt

	def findDivisors(x: Number): Stream[Number] = {
		def findRemainingDivisors(x: Number, current: Number): Stream[Number] = {
			//println("findRemainingDivisors(x: " + x + ", current: " + current + ")")
			if (current > x) {
				Stream.empty
			} else {
				if ((x % current) == BigInt(0)) {
					current #:: findRemainingDivisors(x, current + 1)
				} else {
					findRemainingDivisors(x, current + 1)
				}
			}
		}
		findRemainingDivisors(x, 1)
	}

	def findMultipliesOf(x: Number): Stream[Number] = {
		def findRemainingMultipliesOf(x: Number, current: Number): Stream[Number] = {
			current #:: findRemainingMultipliesOf(x, current + x)
		}
		findRemainingMultipliesOf(x, 0)
	}

	lazy val fibonacci: Stream[Number] = {
		def nextVal(v1: Number, v2: Number): Stream[Number] = v1 #:: nextVal(v2, v1 + v2)
		nextVal(1, 1)
	}

	private def from(x: Number): Stream[Number] = x #:: from(x + 1)

	def naturals: Stream[Number] = from(0)

	lazy val primes: Stream[Number] = {
		def sieve(s: Stream[Number]): Stream[Number] = s.head #:: sieve(s.tail filter (_ % s.head != 0))
		sieve(from(2))
	}

	def primesWithMax(max: Number): Stream[Number] = {
		def sieve(s: Stream[Number]): Stream[Number] = {
			if (s.isEmpty) {
				Stream.Empty
			} else {
				s.head #:: sieve(s.tail filter (_ % s.head != 0))
			}
		}
		sieve((BigInt(2) to max).toStream)
	}

	lazy val primes2: Stream[Number] = {
		lazy val init = from(1).map(x => (x, false))
		//println("init: " + init.toList)
		def stage123(x: Stream[(Number, Boolean)]): Stream[(Number, Boolean)] = {
			val rs1 = List(1, 13, 17, 29, 37, 41, 49, 53)
			val rs2 = List(7, 19, 31, 43)
			val rs3 = List(11, 23, 47, 59)
			lazy val stage1 = x.map(t => (t._1, if (rs1.contains(t._1 % 60) && solve1(t._1)) !t._2 else t._2))
			//println("stage1: " + stage1.toList)
			lazy val stage2 = stage1.map(t => (t._1, if (rs2.contains(t._1 % 60) && solve2(t._1)) !t._2 else t._2))
			//println("stage2: " + stage2.toList)
			lazy val stage3 = stage2.map(t => (t._1, if (rs3.contains(t._1 % 60) && solve3(t._1)) !t._2 else t._2))
			stage3
		}
		def solve1(n: Number): Boolean = {
			(for {
				x <- BigInt(1) to floorSqrt(n / 4)
				y <- BigInt(1) to floorSqrt(n)
				if 4 * x * x + y * y == n
			} yield {
				1
			}).length % 2 != 0
		}
		def solve2(n: Number): Boolean = {
			(for {
				x <- BigInt(1) to floorSqrt(n / 3)
				y <- BigInt(1) to floorSqrt(n)
				if 3 * x * x + y * y == n
			} yield {
				1
			}).length % 2 != 0
		}
		def solve3(n: Number): Boolean = {
			(for {
				x <- BigInt(1) to floorSqrt(n)
				y <- BigInt(1) until x
				if 3 * x * x - y * y == n
			} yield {
				1
			}).length % 2 != 0
		}
		lazy val stage2 = stage123(init).filter(_._2).map(_._1)
		//println("stage22: " + stage2.toList)
		def sieve(s: Stream[Number]): Stream[Number] = {
			if (s.isEmpty) {
				Stream.empty
			} else {
				s.head #:: sieve(s.tail.filter(_ % s.head.pow(2) != 0))
			}
		}
		2 #:: 3 #:: 5 #:: sieve(stage2)
	}

	def primes2WithMax(max: Number): Stream[Number] = {
		val init = (BigInt(1) to max).map(x => (x, false)).toStream
		def stage123(x: Stream[(Number, Boolean)]): Stream[(Number, Boolean)] = {
			val rs1 = List(1, 13, 17, 29, 37, 41, 49, 53)
			val rs2 = List(7, 19, 31, 43)
			val rs3 = List(11, 23, 47, 59)
			lazy val stage1 = x.map(t => (t._1, if (rs1.contains(t._1 % 60) && solve1(t._1)) !t._2 else t._2))
			//println("stage1: " + stage1.toList)
			lazy val stage2 = stage1.map(t => (t._1, if (rs2.contains(t._1 % 60) && solve2(t._1)) !t._2 else t._2))
			//println("stage2: " + stage2.toList)
			lazy val stage3 = stage2.map(t => (t._1, if (rs3.contains(t._1 % 60) && solve3(t._1)) !t._2 else t._2))
			stage3
		}
		def solve1(n: Number): Boolean = {
			(for {
				x <- BigInt(1) to floorSqrt(n / 4)
				y <- BigInt(1) to floorSqrt(n)
				if 4 * x * x + y * y == n
			} yield {
				1
			}).length % 2 != 0
		}
		def solve2(n: Number): Boolean = {
			(for {
				x <- BigInt(1) to floorSqrt(n / 3)
				y <- BigInt(1) to floorSqrt(n)
				if 3 * x * x + y * y == n
			} yield {
				1
			}).length % 2 != 0
		}
		def solve3(n: Number): Boolean = {
			(for {
				x <- BigInt(1) to floorSqrt(n)
				y <- BigInt(1) until x
				if 3 * x * x - y * y == n
			} yield {
				1
			}).length % 2 != 0
		}
		lazy val stage2 = stage123(init).filter(_._2).map(_._1)
		def sieve(s: Stream[Number]): Stream[Number] = {
			if (s.isEmpty) {
				Stream.empty
			} else {
				s.head #:: sieve(s.tail.filter(_ % s.head.pow(2) != 0))
			}
		}
		2 #:: 3 #:: 5 #:: sieve(stage2)
	}

	def floorSqrt(n: Number): Number = BigDecimal(sqrt(n.toDouble).floor.toString).toBigInt()

	def timesIn(n: Number, x: Number): Number = {
		def timesIn(n: Number, x: Number, acc: Number): Number = {
			if (n % x == BigInt(0)) {
				timesIn(n / x, x, acc + 1)
			} else {
				acc
			}
		}
		if (x == BigInt(0) || x == BigInt(1)) {
			throw new IllegalArgumentException
		}
		timesIn(n, x, 0)
	}

	def factorial(n: Number): Number = {
		def f(n: Number, acc: Number): Number = {
			if (n < 1) throw new IllegalArgumentException("Cannot calculate factorial for something less than 1")
			if (n == BigInt(1)) acc else f(n - 1, acc * n)
		}
		f(n, 1)
	}

	// returns Stream of pairs (a, b), where a is a prime factor and b is count of that prime factor
	def factorize(n: Number): Stream[(Number, Int)] = {
		def factorize(x: Number, primes: Stream[Number], currCount: Int, res: Stream[(Number, Int)]): Stream[(Number, Int)] = {
			//println("factorize(x: " + x + ", primes.head: " + primes.head + ", currCount: " + currCount + ")")
			if (x == BigInt(1)) {
				(primes.head, currCount) #:: res
			} else {
				if (x % primes.head == BigInt(0)) {
					factorize(x / primes.head, primes, currCount + 1, res)
				} else {
					factorize(x, primes.tail, 0, (primes.head, currCount) #:: res)
				}
			}
		}
		factorize(n, primes, 0, Stream.empty)
	}

	def sumFactors(x: Stream[(Number, Int)], y: Stream[(Number, Int)]): Stream[(Number, Int)] = {
		def add(x: Stream[(Number, Int)], res: Map[Number, Int]): Map[Number, Int] = {
			if (x.isEmpty) {
				res
			} else {
				add(x.tail, res.updated(x.head._1, res.getOrElse(x.head._1, 0) + x.head._2))
			}
		}
		add(x, add(y, new HashMap[Number, Int])).toStream
	}

	def textualRepresentation(n: Number): String = {
		if (n / 1000 > 9) {
			throw new IllegalArgumentException("Numbers supported only up to 9999")
		}
		val t = if (n / 1000 > 0) textualRepresentation(n / 1000) + " thousand" else ""
		val h = if ((n % 1000) / 100 > 0) textualRepresentation((n % 1000) / 100) + " hundred" else ""
		val th = t + (if (t.length > 0 && h.length > 0) " " else "") + h
		val d = if ((n % 100) / 10 > 1) {
			(((n % 100) / 10).intValue() match {
				case 2 => "twenty"
				case 3 => "thirty"
				case 4 => "forty"
				case 5 => "fifty"
				case 6 => "sixty"
				case 7 => "seventy"
				case 8 => "eighty"
				case 9 => "ninety"
				case _ => throw new IllegalStateException()
			}) + (if (textualRepresentation(n % 10).length > 0) "-" else "") + textualRepresentation(n % 10)
		} else {
			(n % 100).intValue() match {
				case 1 => "one"
				case 2 => "two"
				case 3 => "three"
				case 4 => "four"
				case 5 => "five"
				case 6 => "six"
				case 7 => "seven"
				case 8 => "eight"
				case 9 => "nine"
				case 10 => "ten"
				case 11 => "eleven"
				case 12 => "twelve"
				case 13 => "thirteen"
				case 14 => "fourteen"
				case 15 => "fifteen"
				case 16 => "sixteen"
				case 17 => "seventeen"
				case 18 => "eighteen"
				case 19 => "nineteen"
				case 0 => ""
				case _ => throw new IllegalStateException()
			}
		}
		val hd = if (th.length > 0 && d.length > 0) " and " else ""
		th + hd + d
	}

	def powerModulo(base: Number, exponent: Number, modulo: Number): Number = {
		def powerModulo0(base: Number, exponent: Number, modulo: Number, acc: Number): Number = {
			if (exponent == BigInt(1)) {
				acc
			} else {
				powerModulo0(base, exponent - 1, modulo, (acc * base) % modulo)
			}
		}
		if (exponent == BigInt(0)) {
			1
		} else {
			powerModulo0(base, exponent, modulo, base)
		}
	}

	def sumOf5thPowersOfDigits(n: Number): Number = {
		(1 to 100).map(x => (n / BigInt(10).pow(x - 1)) % 10).map(x => x.pow(5)).sum
	}

	def multiGcd(numbers: Stream[Number]): Number = numbers.foldRight(numbers(0))((x, b) => x.gcd(b))
}
