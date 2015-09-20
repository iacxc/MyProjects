
import payroll.api._
import payroll.api.DeductionsCalculator._
import payroll._
import payroll.Money.Type2Money._

val buck = Employee(Name("Buck", "Trends"), Money(80000))
val jane = Employee(Name("Jane", "Doe"), Money(90000))

List(buck, jane).foreach { employee =>
  val biweeklyGross = employee.annualGrossSalary / 26.0
  val deductions = federalIncomeTax(employee, biweeklyGross) +
      stateIncomeTax(employee, biweeklyGross) +
      insurancePremiums(employee, biweeklyGross) +
      retirementFundContributions(employee, biweeklyGross)

  val check = PayCheck(biweeklyGross, biweeklyGross - deductions, deductions)

  println(String.format(
    "%s %s: %s", employee.name.first, employee.name.last, check))
}


