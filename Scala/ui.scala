

package ui

import observer._

class Button(val label: String) extends Widget {
    def click() = {

    }
}

abstract class Widget {
    class Properties {
        import scala.collection.immutable.HashMap

        private var values: Map[String, Any] = new HashMap

        def size = values.size
        def get(key: String) = values.get(key)
        def update(key: String, value: Any) = {
            values = values.updated(key, value)
        }
    }

    val properties = new Properties
}

class ButtonWithCallbacks(val label: String,
           val clickedCallBacks: List[() => Unit]) extends Widget {
    require(clickedCallBacks != null, "Callback list can't be null")

    def this(label: String, clickedCallBack: () => Unit) =
        this(label, List(clickedCallBack))
    def this(label: String) = {
        this(label, Nil)
        println("Warning: button has no click callbacks!")
    }

    def click() = {
        clickedCallBacks.foreach(f => f())
    }
}

class RadioButtonWithCallbacks(
    var on: Boolean, label: String, clickedCallBacks: List[() => Unit])
        extends ButtonWithCallbacks(label, clickedCallBacks) {

    def this(on: Boolean, label: String, clickedCallBack: () => Unit) =
        this(on, label, List(clickedCallBack))

    def this(on: Boolean, label: String) = this(on, label, Nil)
}

class ObserverableButton(name: String) extends Button(name) with Subject {
     override def click() = {
         super.click()
         notifyObservers
     }
}

class ButtonCountObserver {
    var count = 0
    def receiveUpdate(subject: Any) = count += 1
}

import org.specs._

object ButtonObserverSpec extends Specification {
    "A Button Observer" should {
        "observer button clicks" in {
            val observerableButton = new ObserverableButton("Okay")
            val buttonObserver = new ButtonCountObserver
            observerableButton.addObserver(buttonObserver)

            for (i <- 1 to 3) observerableButton.click()
            buttonObserver.count mustEqual 3
        }
    }
}

object ButtonObserverAnonSpec extends Specificatin {
    "A Button Observer" should {
        "observe button clicks" in {
            val observerableButton = new Button("Okay") with Subject {
                override def click() = {
                    super.click()
                    notifyObservers
                }
            }

            for (i <- 1 to 3) observerableButton.click()
            buttonObserver.count mustEqual 3
        }
    }
}

