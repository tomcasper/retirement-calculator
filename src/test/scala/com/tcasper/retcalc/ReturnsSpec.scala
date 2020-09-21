package com.tcasper.retcalc

import org.scalactic.{Equality, TolerantNumerics, TypeCheckedTripleEquals}
import org.scalatest.matchers.should
import org.scalatest.wordspec._

class ReturnsSpec extends AnyWordSpec with should.Matchers with TypeCheckedTripleEquals {

  "VariableReturns.fromUntil" should {
    "keep only a window of the returns" in {
      val variableReturns = VariableReturns(Vector.tabulate(12) { i =>
        val d = (i + 1).toDouble
        VariableReturn(f"2017.$d%02.0f", d)
      })

      variableReturns.fromUntil("2017.07", "2017.09").returns should ===
      (Vector(
        VariableReturn("2017.07", 7.0),
        VariableReturn("2017.08", 8.0)
      ))

      variableReturns.fromUntil("2017.10", "2018.01").returns should ===
      (Vector(
        VariableReturn("2017.10", 10.0),
        VariableReturn("2017.11", 11.0),
        VariableReturn("2017.12", 12.0)
      ))
    }
  }

}
