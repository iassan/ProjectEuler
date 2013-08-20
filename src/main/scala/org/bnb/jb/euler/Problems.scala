package org.bnb.jb.euler

import org.bnb.jb.euler.common.Numbers._
import scala.io.Source
import java.net.URI
import java.util.Date
import scala.collection.immutable.{HashMap, HashSet}

/**
 * @author Jacek Bilski
 * @version $Revision$
 *          $Id$
 */
class Problems {

	def solve0001: Number = {
		val multiplies = findMultipliesOf(3).takeWhile(_ < 1000).toSet ++ findMultipliesOf(5).takeWhile(_ < 1000).toSet
		//println(multiplies)
		//multiplies.foldRight(BigDecimal(0))(_ + _)
		multiplies.sum
	}

	def solve0002: Number = {
		fibonacci.takeWhile(_ <= 4000000).filter(_ % 2 == 0).sum
	}

	def solve0003: Number = {
		def divide(n: Number, p: Stream[Number], currMax: Number): Number = {
			if (n < currMax) {
				currMax
			}
			else {
				if (p.isEmpty) {
					if (n > currMax) n else currMax
				} else {
					if (n % p.head == 0) {
						divide(n / p.head, p, p.head)
					}
					else {
						divide(n, p.tail, currMax)
					}
				}
			}
		}
		val k = BigInt("600851475143")
		divide(k, primes.takeWhile(_ < k), 1)
	}

	def solve0004: Number = {
		def isPalindrome(n: Number): Boolean = {
			val s = n.toString()
			s.equals(s.reverse)
		}
		(for {
			a <- 999 to 100 by -1
			b <- a to 100 by -1
			if isPalindrome(a * b)
		} yield {
			a * b
		}).max
	}

	def solve0005: Number = {
		val k = 20
		primes.takeWhile(_ < k).map(p => (p, (1 to k).map(timesIn(_, p)).max)).foldRight(BigInt(1))((t, b) => t._1.pow(t._2.intValue()) * b)
	}

	def solve0006: Number = {
		val sumOfSquares = (1 to 100).map(x => x * x).sum
		val sum = (1 to 100).sum
		sum * sum - sumOfSquares
	}

	def solve0007: Number = {
		val p = new Array[Boolean](110000)
		for (i <- 0 until p.length) {
			p(i) = true
		}
		p(0) = false
		p(1) = false
		for (i <- 2 until p.length) {
			if (p(i)) {
				for (j <- 2 * i until p.length by i) {
					p(j) = false
				}
			}
		}
		val primes = p.zipWithIndex.filter(_._1)
		println(primes)
		println(primes.length)
		primes(10000)._2 // 10000 bo liczymy od zera
	}

	def solve0008(filename: URI): Number = {
		val digits = Source.fromFile(filename).iter.toList.map(_.getNumericValue)
		def findMaxProduct(i: Int, currMax: Number): Number = {
			if (i <= digits.length - 5) {
				findMaxProduct(i + 1, currMax.max(digits(i) * digits(i + 1) * digits(i + 2) * digits(i + 3) * digits(i + 4)))
			} else {
				currMax
			}
		}
		findMaxProduct(0, 0)
	}

	def solve0009: Number = {
		// TODO - brutalne podejście, niedobre
		val res = for {
			a <- 1 to 1000
			b <- a + 1 to 1000
			c <- b + 1 to 1000
			if c < a + b
			if a * a + b * b == c * c
			if a + b + c == 1000
		} yield {
			a * b * c
		}
		res(0)
	}

	def solve0010: Number = {
		val p = new Array[Boolean](2000000)
		for (i <- 0 until p.length) {
			p(i) = true
		}
		p(0) = false
		p(1) = false
		for (i <- 2 until p.length) {
			if (p(i)) {
				for (j <- 2 * i until p.length by i) {
					p(j) = false
				}
			}
		}
		val primes = p.zipWithIndex.filter(_._1)
		primes.map(x => BigInt(x._2)).sum
	}

	def solve0011(filename: URI): Number = {
		val problem = Source.fromFile(filename).getLines().toList.map(_.split(" ").map(BigInt(_)).toList)
		def combinations(width: Int, height: Int): Stream[((Int, Int), (Int, Int), (Int, Int), (Int, Int))] = {
			(for {
				x <- 0 until (width - 3)
				y <- 0 until height
			} yield {
				((x, y), (x + 1, y), (x + 2, y), (x + 3, y))
			}).toStream ++
							(for {
								x <- 0 until width
								y <- 0 until (height - 3)
							} yield {
								((x, y), (x, y + 1), (x, y + 2), (x, y + 3))
							}).toStream ++
							(for {
								x <- 0 until (width - 3)
								y <- 0 until (height - 3)
							} yield {
								((x, y), (x + 1, y + 1), (x + 2, y + 2), (x + 3, y + 3))
							}).toStream ++
							(for {
								x <- 3 until width
								y <- 0 until (height - 3)
							} yield {
								((x, y), (x - 1, y + 1), (x - 2, y + 2), (x - 3, y + 3))
							}).toStream
		}
		def findMaxProduct(c: Stream[((Int, Int), (Int, Int), (Int, Int), (Int, Int))], currMax: Number): Number = {
			if (c.isEmpty) {
				currMax
			}
			else {
				val currProduct = problem(c.head._1._1)(c.head._1._2) * problem(c.head._2._1)(c.head._2._2) * problem(c.head._3._1)(c.head._3._2) * problem(c.head._4._1)(c.head._4._2)
				findMaxProduct(c.tail, currMax.max(currProduct))
			}
		}
		//println(combinations(20, 20).toList)
		findMaxProduct(combinations(20, 20), 0)
	}

	def solve0012: Number = {
		val requiredCount = 500
		// rozłożyć na czynniki pierwsze i policzyć liczbę kombinacji tychże
		def countDivisorsOfTriangleOf(n: Number, cod: Map[Number, Number]): Number = {
			def countDivisorsOf(n: Number): Number = factorize(n).foldLeft(BigInt(1))((b, x) => b * (x._2 + 1))
			if (n % 2 == 0) {
				val cod1 = if (cod.contains(n / 2)) cod else cod.updated(n / 2, countDivisorsOf(n / 2))
				val cod2 = if (cod1.contains(n + 1)) cod1 else cod1.updated(n + 1, countDivisorsOf(n + 1))
				if (cod2.getOrElse(n / 2, BigInt(1)) * cod2.getOrElse(n + 1, BigInt(1)) > requiredCount) {
					n
				} else {
					countDivisorsOfTriangleOf(n + 1, cod2)
				}
			} else {
				val cod1 = if (cod.contains(n)) cod else cod.updated(n, countDivisorsOf(n))
				val cod2 = if (cod1.contains((n + 1) / 2)) cod1 else cod1.updated((n + 1) / 2, countDivisorsOf((n + 1) / 2))
				if (cod2.getOrElse(n, BigInt(1)) * cod2.getOrElse((n + 1) / 2, BigInt(1)) > requiredCount) {
					n
				} else {
					countDivisorsOfTriangleOf(n + 1, cod2)
				}
			}
		}
		val res = countDivisorsOfTriangleOf(1, new HashMap[Number, Number])
		res * (res + 1) / 2
	}

	def solve0013(filename: URI): Number = {
		val numbers = Source.fromFile(filename).getLines().toList.map(BigInt(_))
		val sum = numbers.sum
		BigInt(sum.toString().substring(0, 10))
	}

	def solve0014: Number = {
		def sequenceLength(n: Number): Number = {
			def sequenceLength0(n: Number, currLength: Number): Number = {
				if (n == 1) {
					currLength
				} else {
					if (n % 2 == 0) {
						sequenceLength0(n / 2, currLength + 1)
					} else {
						sequenceLength0(3 * n + 1, currLength + 1)
					}
				}
			}
			sequenceLength0(n, 0)
		}
		(1 to 1000000).map(x => (x, sequenceLength(x))).maxBy(_._2)._1
	}

	def solve0015: Number = {
		// to jest po prostu liczba permutacji ciagu o długości x + y (szerokość i wysokość siatki), który zawiera dokładnie x elementów o wartości A i y elementów o wartości B
		lazy val fac20 = factorial(20)
		lazy val fac40 = factorial(40)
		fac40 / (fac20 * fac20)
	}

	def solve0016: Number = (BigInt(1) << 1000).toString().toCharArray.map(_.getNumericValue).sum

	def solve0017: Number = (1 to 1000).map(textualRepresentation(_).replaceAll(" ", "").replaceAll("-", "").length).sum

	def solve0018(filename: URI): Number = {
		val problem = Source.fromFile(filename).getLines().toList.map(_.split(" ").map(BigInt(_)).toList).reverse
		def findMax(a: List[Number], p: List[List[Number]]): Number = {
			// a is longer than b and already contains max sum of tree below
			if (p.isEmpty) {
				a(0)
			}
			else {
				val newA = for {
					j <- 0 until p.head.length
				} yield {
					p.head(j) + a(j).max(a(j + 1))
				}
				findMax(newA.toList, p.tail)
			}
		}
		val start = new Date()
		val res = findMax(problem.head, problem.tail)
		val end = new Date()
		println("runtime (pure): " + (end.getTime - start.getTime) + " ms")
		res
	}

	def solve0019: Number = {
		def days: Stream[(Int, Int, Int, Int)] = {
			// dzień, miesiąc, rok, dzień tygodnia (niedziela = 0)
			val _30dayMonths = Set(4, 6, 9, 11)
			val _31dayMonths = Set(1, 3, 5, 7, 8, 10, 12)
			def isLeapYear(year: Int) = ((year % 4 == 0) && (year % 100 != 0)) || (year % 400 == 0)
			def daysInMonth(month: Int, year: Int) = {
				if (_30dayMonths.contains(month)) {
					30
				}
				else {
					if (_31dayMonths.contains(month)) {
						31
					}
					else {
						if (isLeapYear(year)) 29 else 28
					}
				}
			}
			def nextDays(day: (Int, Int, Int, Int)): Stream[(Int, Int, Int, Int)] = {
				if (day._1 + 1 <= daysInMonth(day._2, day._3)) {
					val newDay = (day._1 + 1, day._2, day._3, (day._4 + 1) % 7)
					newDay #:: nextDays(newDay)
				} else {
					if (day._2 + 1 <= 12) {
						val newDay = (1, day._2 + 1, day._3, (day._4 + 1) % 7)
						newDay #:: nextDays(newDay)
					} else {
						val newDay = (1, 1, day._3 + 1, (day._4 + 1) % 7)
						newDay #:: nextDays(newDay)
					}
				}
			}
			val first = (1, 1, 1900, 1)
			first #:: nextDays(first)
		}
		days.filter(x => x._1 == 1 && x._4 == 0 && x._3 >= 1901).takeWhile(_._3 <= 2000).size
	}

	def solve0020: Number = factorial(100).toString().toCharArray.map(_.getNumericValue).sum

	def solve0021: Number = {
		def d(x: Number): Number = findDivisors(x).filter(_ < x).sum
		val amicables = (BigInt(1) to 10000).map(x => (x, d(x))).toMap
		amicables.keySet.filter(x => amicables.getOrElse(amicables.getOrElse(x, -1), -1) == x && amicables.getOrElse(x, -1) != x).sum
	}

	def solve0022(filename: URI): Number = {
		def alphaValue(s: String): Number = {
			if (s.isEmpty) {
				0
			}
			else {
				s.head.toInt - 64 + alphaValue(s.tail)
			}
		}
		Source.fromFile(filename).getLines().flatMap(_.replaceAll("\"", "").split(",")).toList.sorted.zipWithIndex.map(x => (x._2 + 1) * alphaValue(x._1)).sum
	}

	def solve0023: Number = {
		val limit = 28123
		// od 1 do limit/2 dodajemy wielokrotności od 2 w górę aż do limit
		// mamy sumę podzielników
		def findSumsOfDivisors: Map[Number, Number] = {
			def findSumsOfDivisors(x: Number, y: Number, sumsOfDivisors: Map[Number, Number]): Map[Number, Number] = {
				if (x > limit)
					sumsOfDivisors
				else {
					if (y > limit) {
						findSumsOfDivisors(x + 1, 2 * (x + 1), sumsOfDivisors)
					} else {
						findSumsOfDivisors(x, y + x, sumsOfDivisors.updated(y, sumsOfDivisors.getOrElse(y, BigInt(1)) + x))
					}
				}
			}
			findSumsOfDivisors(2, 4, new HashMap[Number, Number])
		}
		val abundantNumbers = findSumsOfDivisors.filter(x => x._2 > x._1).map(_._1).toSet
		val numbersThatAreSumsOfTwoAbundantNumbers = (for {
			i <- abundantNumbers
			j <- abundantNumbers
			if j >= i
			if i + j <= limit
		} yield {
			i + j
		}).toSet
		val sumOfThoseNumbers = numbersThatAreSumsOfTwoAbundantNumbers.sum
		(1 to limit).sum - sumOfThoseNumbers
	}

	def solve0024: Number = {
		val digits = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
		val permutationNumber = 1000000
		//digits.permutations.toList(permutationNumber - 1).foldLeft(BigInt(0))(10 * _ + _)
		/*
		Idea - starting from left to right we count how many permutations we can have on the right hand side if we keep given position and left side untouched.
		Having that we can set current position and recursively go deeper excluding element at given position
		*/
		def howManyPermutations(elements: Number): Number = factorial(elements)
		def generateNthPermutation(elements: List[Int], permNumber: Number): List[Int] = {
			if (elements.isEmpty) {
				Nil
			} else {
				if (elements.size == 1) {
					List(elements.head)
				} else {
					val x = howManyPermutations(elements.size - 1)
					val elementAtCurrentPosition = elements((permNumber / x).toInt)
					elementAtCurrentPosition :: generateNthPermutation(elements.filter(_ != elementAtCurrentPosition), permNumber % x)
				}
			}
		}
		generateNthPermutation(digits, permutationNumber - 1).foldLeft(BigInt(0))(10 * _ + _)
	}

	def solve0025: Number = fibonacci.takeWhile(_.toString().length <= 1000).zipWithIndex.filter(_._1.toString().length == 1000)(0)._2 + 1

	def solve0028: Number = {
		val size = 1001
		naturals.filter(_ != 0).takeWhile(_ <= size / 2).flatMap(x => List(2 * x, 2 * x, 2 * x, 2 * x)).foldLeft((BigInt(1), BigInt(1)))((s, x) => (s._1 + x, s._2 + s._1 + x))._2
	}

	def solve0029: Number = {
		val limit = 100
		/*
		 Idea: We don't need to calculate those powers. x^y = a^b <=> x=a^w & y=b/w.
		 So for each pair (a,b) we try to look for another pair, that would give the same value and mark it as "reached".
		 And for the same pair, if it wasn't "reached" we add one to counter. In the end the counter is the solution.
		  */
		def s(a: Number, b: Number, counter: Number, reached: Map[Number, Set[Number]]): Number = {
			def findAllCombinations(a: Number, b: Number): Set[(Number, Number)] = {
				// make (64,80) <-> (32,96), (81,75) <-> (27,100), (2,4) <-> (4,2), (10,10) <-> (100,5), etc.
				// So, for example if I have (2^5)^96, then (2^6)^(96*(5/6)).
				// In general (w^x)^b = (w^y)^(b*(x/y)) if b*(x/y) is integer
				def findWandX(aFactors: Stream[(Number, Int)]): (Number, Number) = {
					if (aFactors.size == 1)
						(aFactors(0)._1, BigInt(aFactors(0)._2))
					else {
						val gcd = multiGcd(aFactors.map(x => BigInt(x._2)))
						(aFactors.map(x => x._1.pow((x._2/gcd).intValue())).foldRight(BigInt(1))(_*_), gcd)
					}
				}
				val aFactors = factorize(a)
				val (w, x) = findWandX(aFactors)
				(for {
					y <- x to limit
					if w.pow(y.intValue()) <= limit
					if (b * x % y) == 0
				} yield {
					(w.pow(y.intValue()), b * x / y)
				}).toSet
			}
			def markOtherReached(cs: Set[(Number, Number)], reached: Map[Number, Set[Number]]): Map[Number, Set[Number]] = {
				if (cs.isEmpty) {
					reached
				} else {
					markOtherReached(cs.tail, reached.updated(cs.head._1, reached.getOrElse(cs.head._1, new HashSet[Number]) + cs.head._2))
				}
			}
			if (a > limit) {
				counter
			} else {
				if (b > limit) {
					s(a + 1, 2, counter, reached)
				} else {
					val beenHere = reached.getOrElse(a, new HashSet[Number]).contains(b)
					val counterInc = if (beenHere) 0 else 1
					val cs = findAllCombinations(a, b)
					val newReached = markOtherReached(cs, reached)
					s(a, b + 1, counter + counterInc, newReached)
				}
			}
		}
		s(2, 2, 0, new HashMap[Number, Set[Number]])
	}

	def solve0030: Number = (10 to 354294).filter(x => sumOf5thPowersOfDigits(x) == x).toList.sum

	def solve0048: Number = (BigInt(1) to 1000).map(x => x.modPow(x, BigInt("10000000000"))).sum % BigInt("10000000000")

	def solve0067(filename: URI): Number = solve0018(filename)

	def solve0107(filename: URI): Number = {
		val s = Source.fromFile(filename).getLines().toList.map(_.split(",").toList)
		val vertices = (for {
			i <- 0 until s.length
			j <- 0 until s(i).length
			if j > i
			if s(i)(j) != "-"
		} yield {
			(i, j, BigInt(s(i)(j)))
		}).sortBy(_._3).toList
		val start = new Date()
		val sum = vertices.map(_._3).sum
		val subGraphs = (0 until s.length).map(x => (x, x)).toMap[Int, Int] // identyfikator grupy -> lista wierzchołków w grupie
		def findMinimalSpanningTree(vertices: List[(Int, Int, Number)], subGraphs: Map[Int, Int]): List[(Int, Int, Number)] = {
			def findMinimalSpanningTree0(vertices: List[(Int, Int, Number)], subGraphs: Map[Int, Int], curr: List[(Int, Int, Number)]): List[(Int, Int, Number)] = {
				//				println("Iteartion")
				//				println("subGraphs: " + subGraphs)
				//				println("vertices.head: " + vertices.head + ", v1: " + subGraphs(vertices.head._1) + ", v2: " + subGraphs(vertices.head._2) + ", diff: " + (subGraphs(vertices.head._2) - subGraphs(vertices.head._1)))
				//				println("curr.length: " + curr.length)
				if (subGraphs.values.toSet.size == 1) {
					curr
				}
				else {
					if (vertices.isEmpty) throw new IllegalStateException("Vertices are empty, yet the algorithm didn't finish...")
					if (subGraphs(vertices.head._1) != subGraphs(vertices.head._2)) {
						//subGraphs.updated(vertices.head._1, subGraphs(vertices.head._1) ++ subGraphs(vertices.head._2)).filter(t => t._1 != vertices.head._2)
						//						println("new vertice")
						findMinimalSpanningTree0(vertices.tail, subGraphs.keys.map(x => (x, if (subGraphs(x) == subGraphs(vertices.head._2)) subGraphs(vertices.head._1) else subGraphs(x))).toMap, vertices.head :: curr)
					} else {
						//						println("not interested")
						findMinimalSpanningTree0(vertices.tail, subGraphs, curr)
					}
				}
			}
			findMinimalSpanningTree0(vertices, subGraphs, Nil)
		}
		val minSum = findMinimalSpanningTree(vertices, subGraphs).map(_._3).sum
		val res = sum - minSum
		val end = new Date()
		println("Running time: " + (end.getTime - start.getTime))
		res
	}
}
