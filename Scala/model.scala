
package scells

import scala.swing._

trait Arithmetic {this: Evaluator =>
    operations += (
        "add" -> { case List(x,y) => x + y },
        "sub" -> { case List(x,y) => x - y },
        "div" -> { case List(x,y) => x / y },
        "mul" -> { case List(x,y) => x * y },
        "mod" -> { case List(x,y) => x % y },
        "sum" -> { xs => (0.0 /: xs)(_ + _) },
        "prod" -> { xs => (1.0 /: xs)(_ * _) }
        )
}

trait Evaluator { this: Model =>
    type Op = List[Double] => Double
    val operations = new collection.mutable.HashMap[String, Op]

    def evaluate(e: Formula): Double = try {
        e match {
            case Coord(row, column) => cells(row)(column).value
            case Number(v) => v
            case Textual(_) => 0
            case Application(function, arguments) =>
                val argvals = arguments flatMap evalList
                operations(function)(argvals)
        }
    }
    catch {
        case ex: Exception => Double.NaN
    }

    private def evalList(e: Formula): List[Double] = e match {
        case Range(_,_) => references(e) map (_.value)
        case _ => List(evaluate(e))
    }

    def references(e: Formula): List[Cell] = e match {
        case Coord(row, column) => List(cells(row)(column))
        case Range(Coord(r1, c1), Coord(r2, c2)) =>
            for (row <- (r1 to r2).toList; column <- c1 to c2)
                yield cells(row)(column)
        case Application(function, arguments) =>
            arguments flatMap references
        case _ => List()
    }
}
class Model(val height: Int, val width: Int)
    extends Evaluator with Arithmetic{
    case class ValueChanged(cell: Cell) extends event.Event
    case class Cell(row: Int, column: Int) extends Publisher {
        private var v: Double = 0

        def value: Double = v

        def value_=(w: Double): Unit = {
            if (!(v == w || v.isNaN && w.isNaN)) {
                v = w
                publish(ValueChanged(this))
            }
        }

        private var f: Formula = Empty

        def formula: Formula = f

        def formula_=(f: Formula): Unit = {
            for (c <- references(formula)) deafTo(c)
            this.f = f
            for (c <- references(formula)) listenTo(c)
            value = evaluate(f)
        }

        override def toString = formula match {
            case Textual(s) => s
            case _ => value.toString
        }

        reactions += {
            case ValueChanged(_) => value = evaluate(formula)
        }
    }

    val cells = Array.ofDim[Cell](height, width)
    for(i <- 0 until height; j <- 0 until width)
        cells(i)(j) = new Cell(i,j)
}
