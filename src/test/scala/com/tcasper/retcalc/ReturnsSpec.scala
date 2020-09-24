package com.tcasper.retcalc

import org.scalactic.{Equality, TolerantNumerics, TypeCheckedTripleEquals}
import org.scalatest.EitherValues
import org.scalatest.matchers.should
import org.scalatest.wordspec._

class ReturnsSpec extends AnyWordSpec with should.Matchers with TypeCheckedTripleEquals with EitherValues {

  "VariableReturns.fromUntil" should {
    "keep only a window of the returns" in {
      val variableReturns = VariableReturns(Vector.tabulate(12) { i =>
        val d = (i + 1).toDouble
        VariableReturn(f"2017.$d%02.0f", d)
      })

      variableReturns.fromUntil("2017.07", "2017.09")
        .returns should ===(Vector(VariableReturn("2017.07", 7.0), VariableReturn("2017.08", 8.0)))

      variableReturns.fromUntil("2017.10", "2018.01")
        .returns should ===(Vector(VariableReturn("2017.10", 10.0), VariableReturn("2017.11", 11.0), VariableReturn("2017.12", 12.0)))
    }
  }

  implicit val doubleEquality: Equality[Double] =
    TolerantNumerics.tolerantDoubleEquality(0.0001)

  "Returns.monthlyRate" should {
    "return a fixed rate for a FixedReturn" in {
      Returns.monthlyRate(FixedReturns(0.04), 0).right.value should ===(0.04 / 12)
      Returns.monthlyRate(FixedReturns(0.04), 10).right.value should ===(0.04 / 12)
    }

    val variableReturns = VariableReturns(Vector(
      VariableReturn("2000.01", 0.1),
      VariableReturn("2000.02", 0.2)))
    "return the nth rate for VariableReturn" in {
      Returns.monthlyRate(variableReturns, 0).right.value should ===(0.1)
      Returns.monthlyRate(variableReturns, 1).right.value should ===(0.2)
    }

    "roll over from the first rate if n > length" in {
      Returns.monthlyRate(variableReturns, 2).left.value should ===(RetCalcError.ReturnMonthOutOfBounds(2, 1))
      Returns.monthlyRate(variableReturns, 3).left.value should ===(RetCalcError.ReturnMonthOutOfBounds(3, 1))
      Returns.monthlyRate(variableReturns, 4).left.value should ===(RetCalcError.ReturnMonthOutOfBounds(4, 1))
    }

    "return the n+offset th rate for OffsetReturn" in {
      val returns = OffsetReturns(variableReturns, 1)
      Returns.monthlyRate(returns, 0).right.value should ===(0.2)
      Returns.monthlyRate(returns, 1).right.value should ===(0.1)
    }
  }

  "Returns.fromEquityAndInflationData" should {
    "compute real total returns from equity and inflation data" in {
      val equities = Vector(
        EquityData("2117.01", 100.0, 10.0),
        EquityData("2117.02", 101.0, 12.0),
        EquityData("2117.03", 102.0, 12.0))

      val inflations = Vector(
        InflationData("2117.01", 100.0),
        InflationData("2117.02", 102.0),
        InflationData("2117.03", 102.0))

      val returns = Returns.fromEquityAndInflationData(equities,
        inflations)
      returns should ===(VariableReturns(Vector(
        VariableReturn("2117.02", (101.0 + 12.0 / 12) / 100.0 - 102.0 /
          100.0),
        VariableReturn("2117.03", (102.0 + 12.0 / 12) / 101.0 - 102.0 /
          102.0))))
    }
  }

}
