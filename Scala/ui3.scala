
package ui3

import ui2._

abstract class Widget {
    def draw(): Unit
    override def toString() = "(widget)"
}

class Button(val label: String) extends Widget with Clickable {
    def click() = {

    }

    def draw() = {

    }

    override def toString() =
        "(button: label=" + label + "," + super.toString() + ")"

}
