
package sleepingbarber


import scala.actors.Actor
import scala.actors.Actor._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


case object Haircut


class Customer(val id: Int) extends Actor {
  var shorn = false

  def act() = {
    loop {
      react {
        case Haircut => {
          shorn = true
          println("[c] customer " + id + " got a haircut")
        }
      }
    }
  }
}


class Barber extends Actor {
  private val random = new Random()

  def helpCustomer(customer: Customer): Unit = {
    if (mailboxSize >= 3) {
      println("[b] no enough seats, turning customer " + customer.id + " away")
    }
    else {
      println("[b] cutting hair of customer " + customer.id)
      Thread.sleep(100 + random.nextInt(400))
      customer ! Haircut
    }
  }

  def act(): Unit = {
    loop {
      react {
        case customer: Customer => helpCustomer(customer)
      }
    }
  }
}


class Shop extends Actor {
  val barber = new Barber()
  barber.start

  def act(): Unit = {
    println("[s] the shop is open")

    loop {
      react {
        case customer: Customer => barber ! customer
      }
    }
  }
}


import scala.collection.{immutable, mutable}

object BarbershopSimulator {
  private val random = new Random()
  private val customers = new ArrayBuffer[Customer]()
  private val shop = new Shop()

  def generateCustomers: Unit = {
    for(i <- 1 to 20) {
      val customer = new Customer(i)
      customer.start()
      customers += customer
    }

    println("[!] generated " + customers.size + " customers")

  }

  def trickleCustomers: Unit = {
    for (customer <- customers) {
      shop ! customer
      Thread.sleep(random.nextInt(250))
    }
  }

  def tallyCuts: Unit = {
    Thread.sleep(2000)

    val shornCount = customers.filter(c => c.shorn).size
    println("[!] " + shornCount + " customers got haircuts today")
  }

  def main(args: Array[String]): Unit = {
    println("[!] starting barbershop simulation")
    shop.start()

    generateCustomers
    trickleCustomers
    tallyCuts

    System.exit(0)
  }
}
