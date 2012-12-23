package org.bnb.jb.euler

import org.scalatest.junit.JUnitSuite
import org.junit.{Ignore, Test}

/**
 * @author Jacek Bilski
 * @version $Revision$
 *          $Id$
 */
class ProblemsTest extends JUnitSuite {

	@Test
	@Ignore
	def testSolve0001() {
		val problem = new Problems
		val result = problem.solve0001
		println("Problem 0001: " + result)
		assert(result == 233168)
	}

	@Test
	@Ignore
	def testSolve0002() {
		val problem = new Problems
		val result = problem.solve0002
		println("Problem 0002: " + result)
		assert(result == 4613732)
	}

	@Test
	@Ignore
	def testSolve0003() {
		val problem = new Problems
		val result = problem.solve0003
		println("Problem 0003: " + result)
		assert(result == 6857)
	}

	@Test
	@Ignore
	def testSolve0004() {
		val problem = new Problems
		val result = problem.solve0004
		println("Problem 0004: " + result)
		assert(result == 232792560)
	}

	@Test
	@Ignore
	def testSolve0005() {
		val problem = new Problems
		val result = problem.solve0005
		println("Problem 0005: " + result)
		assert(result == 232792560)
	}

	@Test
	@Ignore
	def testSolve0006() {
		val problem = new Problems
		val result = problem.solve0006
		println("Problem 0006: " + result)
		assert(result == 25164150)
	}

	@Test
	//@Ignore("doesn't work")
	def testSolve0007() {
		val problem = new Problems
		val result = problem.solve0007
		println("Problem 0007: " + result)
		assert(result == 104743)
	}

	@Test
	@Ignore
	def testSolve0008() {
		val problem = new Problems
		val result = problem.solve0008(getClass.getResource("../../../../0008/number.txt").toURI)
		println("Problem 0008: " + result)
		assert(result == BigInt("40824"))
	}

	@Test
	@Ignore
	def testSolve0009() {
		val problem = new Problems
		val result = problem.solve0009
		println("Problem 0009: " + result)
		assert(result == 31875000)
	}

	@Test
	@Ignore("doesn't work")
	def testSolve0010() {
		val problem = new Problems
		val result = problem.solve0010
		println("Problem 0010: " + result)
		assert(result == 31875000)
	}

	@Test
	@Ignore
	def testSolve0011() {
		val problem = new Problems
		val result = problem.solve0011(getClass.getResource("../../../../0011/grid.txt").toURI)
		println("Problem 0011: " + result)
		assert(result == 70600674)
	}

	@Test
	@Ignore
	def testSolve0012() {
		val problem = new Problems
		val result = problem.solve0012
		println("Problem 0012: " + result)
		assert(result == 76576500)
	}

	@Test
	@Ignore
	def testSolve0013() {
		val problem = new Problems
		val result = problem.solve0013(getClass.getResource("../../../../0013/numbers.txt").toURI)
		println("Problem 0013: " + result)
		assert(result == BigInt("5537376230"))
	}

//	@Test
//	//@Ignore
//	def testSolve0014() {
//		val problem = new Problems
//		val result = problem.solve0014
//		println("Problem 0014: " + result)
//		assert(result == -1)
//	}

	@Test
	@Ignore
	def testSolve0016() {
		val problem = new Problems
		val result = problem.solve0016
		println("Problem 0016: " + result)
		assert(result == 1366)
	}

	@Test
	@Ignore
	def testSolve0017() {
		val problem = new Problems
		val result = problem.solve0017
		println("Problem 0017: " + result)
		assert(result == 21124)
	}

	@Test
	@Ignore
	def testSolve0018() {
		val problem = new Problems
		val result = problem.solve0018(getClass.getResource("../../../../0018/triangle.txt").toURI)
		println("Problem 0018: " + result)
		assert(result == 1074)
	}

	@Test
	@Ignore
	def testSolve0020() {
		val problem = new Problems
		val result = problem.solve0020
		println("Problem 0020: " + result)
		assert(result == 648)
	}

//	@Test
//	@Ignore
//	def testSolve0025() {
//		val problem = new Problems
//		val result = problem.solve0025
//		println("Problem 0025: " + result)
//		assert(result == 1)
//	}

	@Test
	@Ignore
	def testSolve0067() {
		val problem = new Problems
		val result = problem.solve0067(getClass.getResource("../../../../0067/triangle.txt").toURI)
		println("Problem 0067: " + result)
		assert(result == 7273)
	}

	@Test
	@Ignore
	def testSolve0107() {
		val problem = new Problems
		val result = problem.solve0107(getClass.getResource("../../../../0107/network.txt").toURI)
		println("Problem 0107: " + result)
		assert(result == 259679)
	}
}
