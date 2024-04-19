package scala.u06.lab

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

import scala.u06.lab.PetriNetFacade.PN
import scala.u06.examples.Pn
import scala.u06.modelling.SystemAnalysis

class PNTest extends  AnyFunSuite:
  import Pn.*
  import Pn.Place.*
  import SystemAnalysis.*
  import PN.*

  test("mutual exlusion test")
    val pnMe = mutualExclusion
    val m = marking(N, N)
    val expected1 = List(marking(N, N), marking(T, N), marking(T, T), marking(C, T), marking(T), marking(C), marking())
    val expected2 = List(marking(N, N), marking(T, N), marking(C, N), marking(C, T), marking(T), marking(C), marking())
    val expected3 = List(marking(N, N), marking(T, N), marking(C, N), marking(N), marking(T), marking(C), marking())

    val res = pnMe.toSystem.paths(m, 7)
    println(res.noTokenPrint)
    // remove empty places from marking
    res.toSet.map(_.map(_.filter(_._2.nonEmpty))) should be(Set(expected1, expected2, expected3))
  
  test("Test readers and writers"):
    val pn = rW.toSystem
    val m = marking("p1", "p1", "p1", "p5")
    val res = pn.paths(m, 7)
    println(res.noTokenPrint)

