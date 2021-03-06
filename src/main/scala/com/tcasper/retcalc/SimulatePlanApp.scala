package com.tcasper.retcalc

object SimulatePlanApp extends App {

  println(strMain(args))

  def strMain(args: Array[String]): String = {
    val (from +: until +: Nil) = args(0).split(",").toList
    val nbOfYearsSaving = args(1).toInt
    val nbOfYearsInRetirement = args(2).toInt

    val allReturns = Returns.fromEquityAndInflationData(
      equities = EquityData.fromResource("sp500_2017.tsv"),
      inflations = InflationData.fromResource("cpi_2017.tsv"))

    RetCalc.simulatePlan(
        returns = allReturns.fromUntil(from, until),
        params = RetCalcParams(
          nbOfMonthsInRetirement = nbOfYearsInRetirement * 12,
          netIncome = args(3).toInt,
          currentExpenses = args(4).toInt,
          initialCapital = args(5).toInt),
        nbOfMonthsSavings = nbOfYearsSaving * 12
    ) match {
      case Right((capitalAtRetirement, capitalAfterDeath)) =>
        s"""
           |Capital after $nbOfYearsSaving years of savings:
            ${capitalAtRetirement.round}
           |Capital after $nbOfYearsInRetirement years in retirement:
            ${capitalAfterDeath.round}
        """.stripMargin

      case Left(err) => err.message
    }
  }
}
