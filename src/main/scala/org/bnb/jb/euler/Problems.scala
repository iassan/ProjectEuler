package org.bnb.jb.euler

import org.bnb.jb.euler.common.Numbers._
import scala.io.Source
import java.net.URI
import java.util.Date
import collection.{immutable, mutable}

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
		println(primes2.take(1000).toList)
		//		val d1 = new Date()
		//		val p1 = primes.take(100).toList
		//		val d2 = new Date()
		//		val p2 = primes2.take(100).toList
		//		val d3 = new Date()
		//		println("p1: " + (d2.getTime - d1.getTime) + " ms, p2: " + (d3.getTime - d2.getTime) + " ms")
		//		println(p1)
		//		println(p2)
		-1
//		primes2(10001)
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

	def solve0010: Number = primes.takeWhile(_ < 2000000).sum

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
		val cod = new mutable.HashMap[Number, Number]
		// rozłożyć na czynniki pierwsze i policzyć liczbę kombinacji tychże
		def countDivisorsOfTriangleOf(n: Number): Number = {
			if (n % 2 == 0) {
				cod.getOrElseUpdate(n / 2, countDivisorsOf(n / 2)) * cod.getOrElseUpdate(n + 1, countDivisorsOf(n + 1))
			} else {
				cod.getOrElseUpdate(n, countDivisorsOf(n)) * cod.getOrElseUpdate((n + 1) / 2, countDivisorsOf((n + 1) / 2))
			}
		}
		def countDivisorsOf(n: Number): Number = {
			def factorize(x: Number, primes: Stream[Number], currCount: Int, res: Number): Number = {
				//println("factorize(x: " + x + ", primes.head: " + primes.head + ", currCount: " + currCount + ")")
				if (x == 1) {
					currCount * res
				} else {
					if (x % primes.head == 0) {
						factorize(x / primes.head, primes, currCount + 1, res)
					}
					else {
						factorize(x, primes.tail, 1, currCount * res)
					}
				}
			}
			factorize(n, primes, 1, 1)
		}
		val res = ((1 to 15000).toStream.map(x => (x, countDivisorsOfTriangleOf(x))).filter(_._2 >= 500))(0)
		triangleNumber(res._1)
	}

	def solve0013(filename: URI): Number = {
		val numbers = Source.fromFile(filename).getLines().toList.map(BigInt(_))
		val sum = numbers.sum
		BigInt(sum.toString().substring(0, 10))
	}

//	def solve0014: Number = {
//		val limit: Number = 1000000
//		val hardLimit: Number = 5000000
//		//		def nextStep(currVal: Number, stepNo: Number, maxVal: Number): Number = {
//		//			println("currVal: " + currVal + ", stepNo: " + stepNo + ", maxVal: " + maxVal)
//		//			if (currVal > hardLimit)
//		//				maxVal
//		//			else {
//		//				if ((currVal > 1) && (currVal - 1) % 3 == 0 && ((currVal - 1) / 3) % 2 == 1)
//		//					nextStep((currVal - 1) / 3, stepNo + 1, if (currVal < limit) currVal else maxVal)
//		//				else
//		//					nextStep(currVal * 2, stepNo + 1, if (currVal < limit) currVal else maxVal)
//		//			}
//		//		}
//		def nextStep(currVals: Set[Number], nextVals: Set[Number], stepNo: Number, m: Map[Number, Number]): Map[Number, Number] = {
//			if (currVals.isEmpty)
//				nextStep(nextVals, Set(), stepNo + 1)
//			else {
//				if ((currVals.head-1)%3==0 && ((currVals.head-1)/3)%2==1 && (currVals.head-1)/3>1)
//
//			}
//		}
//		nextStep(Set(1), Set.empty, 0, Map.empty)
//	}

	def solve0016: Number = (BigInt(1) << (1000)).toString().toCharArray.map(_.getNumericValue).sum

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

	def solve0020: Number = factorial(100).toString().toCharArray.map(_.getNumericValue).sum

	//	def solve0025: Number = {
	//		fibonacci.filter(_.toString().length <= 1000).take(1).toList
	//	}

	def solve0028: Number = {
		val size = 1001
		naturals.filter(_ != 0).takeWhile(_ <= size / 2).toList.flatMap(x => List(2 * x, 2 * x, 2 * x, 2 * x)).foldLeft((BigInt(1), BigInt(1)))((s, x) => (s._1 + x, s._2 + s._1 + x))._2
	}

	def solve0030: Number = {
		val numbers = (10 to 354294).filter(x => sumOf5thPowersOfDigits(x) == x).toList
		println(numbers)
		numbers.sum
	}

	def solve0048: Number = {
		//(1 to 1000).map(x => powerModulo(x, x, BigInt("10000000000"))).sum % BigInt("10000000000")
		(BigInt(1) to 1000).map(x => x.modPow(x, BigInt("10000000000"))).sum % BigInt("10000000000")
	}

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
		def findMinimalSpanningTree(vertices: List[(Int, Int, Number)], subGraphs: immutable.Map[Int, Int]): List[(Int, Int, Number)] = {
			def findMinimalSpanningTree0(vertices: List[(Int, Int, Number)], subGraphs: immutable.Map[Int, Int], curr: List[(Int, Int, Number)]): List[(Int, Int, Number)] = {
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
