package element

object Element {
    private class ArrayElement (
        val contents: Array[String]
    ) extends Element

    private class LineElement(s: String) extends Element {
        val contents = Array(s)
        override def width = s.length
        override def height = 1
    }

    private class UniformElement (
        ch: Char,
        override val width: Int,
        override val height: Int
    ) extends Element {
        private val line = ch.toString * width

        def contents = Array.fill(height)(line)
    }

    def apply(contents: Array[String]): Element = new ArrayElement(contents)

    def apply(chr: Char, width: Int, height: Int): Element = {
        if (width < 0 || height < 0) {
            throw new IllegalArgumentException
        }
        new UniformElement(chr, width, height)
    }

    def apply(line: String): Element = new LineElement(line)
}


abstract class Element {
    def contents: Array[String]
    def height: Int = contents.length
    def width: Int = if (height == 0) 0 else contents(0).length

    def above(that: Element): Element = {
        val this1 = this widen that.width
        val that1 = that widen this.width
        Element(this1.contents ++ that1.contents)
    }

    def beside(that: Element): Element = {
        val this1 = this heighten that.height
        val that1 = that heighten this.height
        Element(
            for((line1, line2) <- this1.contents zip that1.contents
            ) yield line1 + line2
        )
    }

    def widen(w: Int): Element =
        if (w <= width) this
        else {
            val left = Element(' ', (w - width) / 2, height)
            val right = Element(' ', w - width - left.width, height)
            left beside this beside right
        } ensuring (w <= _.width)

    def heighten(h: Int): Element =
        if (h <= height) this
        else {
            val top = Element(' ', width, (h - height) / 2)
            val bot = Element(' ', width, h - height - top.height)
            top above this above bot
        }

    override def toString = contents mkString "\n"
}

import org.specs._
object ElementSpecification extends Specification {
    "A Uniform Element" should {
        "have a width equal to the passed value" in {
            val ele = Element('x', 2, 3)
            ele.width must be_==(2)
        }
        "have a height equal to the passed value" in {
            val ele = Element('x', 2, 3)
            ele.height must be_==(3)
        }
        "throw an IAE if passed a negative width" in {
            Element('x', -2, 3) must throwA[IllegalArgumentException]
        }
    }
}
