
package payroll

case class PayCheck(gross: Money, net: Money, deductions: Money) {
  def plusGross(m: Money)      = PayCheck(gross + m, net + m, deductions)
  def plusDeductions(m: Money) = PayCheck(gross,     net - m, deductions + m)
}

case class Name(first: String, last: String)
case class Employee(name: Name, annualGrossSalary: Money)

import java.math.{BigDecimal => JBigDecimal, MathContext => JMathContext,
                  RoundingMode => JRoundingMode}

class Money(val amount: BigDecimal) {
  def + (m: Money) = Money(amount.bigDecimal.add(m.amount.bigDecimal))
  def - (m: Money) = Money(amount.bigDecimal.subtract(m.amount.bigDecimal))
  def * (m: Money) = Money(amount.bigDecimal.multiply(m.amount.bigDecimal))
  def / (m: Money) = Money(amount.bigDecimal.divide(m.amount.bigDecimal,
                           Money.scale, Money.jroundingMode))

  def <  (m: Money) = amount <  m.amount
  def <= (m: Money) = amount <= m.amount
  def >  (m: Money) = amount >  m.amount
  def >= (m: Money) = amount >= m.amount

  override def equals(o: Any) = o match {
    case m: Money => amount equals m.amount
    case _ => false
  }

  override def hashCode = amount.hashCode * 31

  override def toString =
        String.format("$%.2f", double2Double(amount.doubleValue))
}

object Money {
  def apply(amount: BigDecimal)  = new Money(amount)
  def apply(amount: JBigDecimal) = new Money(scaled(new BigDecimal(amount)))
  def apply(amount: Double)      = new Money(scaled(BigDecimal(amount)))
  def apply(amount: Long)        = new Money(scaled(BigDecimal(amount)))
  def apply(amount: Int)         = new Money(scaled(BigDecimal(amount)))

  def unapply(m: Money) = Some(m.amount)

  protected def scaled(d: BigDecimal) = d.setScale(scale, roundingMode)

  val scale = 4
  val jroundingMode = JRoundingMode.HALF_UP
  val roundingMode = BigDecimal.RoundingMode.HALF_UP
  val context = new JMathContext(scale, jroundingMode)

  import scala.language.implicitConversions
  object Type2Money {
    implicit def bigDecimal2Money(b: BigDecimal)   = Money(b)
    implicit def jBigDecimal2Money(b: JBigDecimal) = Money(b)
    implicit def double2Money(b: Double)           = Money(b)
    implicit def long2Money(b: Long)               = Money(b)
    implicit def int2Money(b: Int)                 = Money(b)
  }
}

package api {

  import Money.Type2Money._

  object DeductionsCalculator {
    def federalIncomeTax(empl: Employee, gross: Money) = gross * 0.25

    def stateIncomeTax(empl: Employee, gross: Money) = gross * 0.05

    def insurancePremiums(empl: Employee, gross: Money) = Money(500)

    def retirementFundContributions(empl: Employee, gross: Money) = gross * 0.10
  }

}
