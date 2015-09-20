/**
 * Created by chengxin on 2015/9/17.
 */


package ui2

trait Clickable { def click() }

import ui.Widget
import observer._

class Button(val label: String) extends Widget with Clickable {
    def click() = {
    }
}

trait ObservableClicks extends Clickable with Subject {
    abstract override def click() = {
        super.click()
        notifyObservers
    }
}

trait VetoableClicks extends Clickable {
    val maxAllowed = 1
    private var count = 0

    abstract override def click() = {
        if (count < maxAllowed) {
            count += 1
            super.click()
        }
    }
}

import org.specs._
import ui.ButtonCountObserver

object ButtonClockableObserverSpec extends Specification {
    "A Button Observer" should {
        "observer button clicks" in {
            val observableButton =
                new Button("Okay") with ObservableClicks
            val buttonClickCountObserver = new ButtonCountObserver
            observableButton.addObserver(buttonClickCountObserver)

            for (i <- 1 to 3) observerableButton.click()
            buttonObserver.count mustEqual 3

        }
    }
}

object ButtonClockableObserverVetoableSpec extends Specification {
    "A Button Observer with Vetoable Clicks" should {
        "observer only the first button clicks" in {
            val observableButton =
                new Button("Okay") with ObservableClicks
                                   with VetoableClicks
            val buttonClickCountObserver = new ButtonCountObserver
            observableButton.addObserver(buttonClickCountObserver)

            for (i <- 1 to 3) observerableButton.click()
            buttonObserver.count mustEqual 1

        }
    }
}



