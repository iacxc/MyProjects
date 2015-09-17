

package observer {

    import scala.language.reflectiveCalls

    trait Subject {
        type Observer = {def receiveUpdate(subject: Any)}

        private var observers = List[Observer]()

        def addObserver(observer: Observer) = observers ::= observer

        def notifyObservers = observers foreach (_.receiveUpdate(this))
    }
}


package ui {

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
                values = values.update(key, value)
            }
        }

        val properties = new Properties
    }

    class ButtonWithCallbacks(val label: String,
               val clickedCallBacks: List[() => Unit]) extends Widget {
        require(clickedCallBacks != null, "Callback list can't be null")

        def this(label: String, clickedCallback: () => Unit) =
            this(label, List(clickedCallBacks))
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

        def this(on: Boolean, label: String, clickedCallback: () => Unit) =
            this(on, label, List(clickedCallBacks))

        def this(on: Boolean, label: String) = this(on, label Nil)
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

                observerableButton.addObserver(buttonObserver)

                for (i <- 1 to 3) observerableButton.click()
                buttonObserver.count mustEqual 3
            }
        }
    }
}


package ui2 {
    import ui.Widget
    import observer._

    trait Clickable { def click() }

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
}

