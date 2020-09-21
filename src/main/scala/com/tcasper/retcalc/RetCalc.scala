package com.tcasper.retcalc

import scala.annotation.tailrec

object RetCalc {

  // Initial futureCapital before refactoring
  //  def futureCapital(interestRate: Double, nbOfMonths: Int, netIncome: Int,
  //                    currentExpenses: Int, initialCapital: Double): Double = {
  //
  //    val monthlySavings = netIncome - currentExpenses
  //    def nextCapital(accumulated: Double, month: Int): Double =
  //      accumulated * (1 + interestRate) + monthlySavings
  //
  //    (0 until nbOfMonths).foldLeft(initialCapital)(nextCapital)
  //  }

  def futureCapital(returns: Returns, nbOfMonths: Int, netIncome: Int,
                    currentExpenses: Int, initialCapital: Double): Double = {
    val monthlySavings = netIncome - currentExpenses
    (0 until nbOfMonths).foldLeft(initialCapital) {
      case (accumulated, month) =>
        accumulated * (1 + Returns.monthlyRate(returns, month)) + monthlySavings
    }
  }

  def simulatePlan(returns: Returns,
                   nbOfMonthsSaving: Int, nbOfMonthsInRetirement: Int,
                   netIncome: Int, currentExpenses: Int, initialCapital:
                   Double): (Double, Double) = {

    val capitalAtRetirement = futureCapital(
      returns, nbOfMonths = nbOfMonthsSaving,
      netIncome = netIncome, currentExpenses = currentExpenses,
      initialCapital = initialCapital)

    val capitalAfterDeath = futureCapital(
      returns, nbOfMonths = nbOfMonthsInRetirement,
      netIncome = 0, currentExpenses = currentExpenses,
      initialCapital = capitalAtRetirement)

    (capitalAtRetirement, capitalAfterDeath)
  }

  def nbOfMonthsSaving(returns: Returns, nbOfMonthsInRetirement: Int,
                       netIncome: Int, currentExpenses: Int, initialCapital: Double): Int = {
    @tailrec
    def loop(months: Int): Int = {
      val (capitalAtRetirement, capitalAfterDeath) = simulatePlan(
        returns,
        nbOfMonthsSaving = months, nbOfMonthsInRetirement =
          nbOfMonthsInRetirement,
        netIncome = netIncome, currentExpenses = currentExpenses,
        initialCapital = initialCapital)

      if (capitalAfterDeath > 0.0)
        months
      else
        loop(months + 1)
    }
    if (netIncome > currentExpenses) {
      loop(0)
    } else {
      Int.MaxValue
    }
  }
}
