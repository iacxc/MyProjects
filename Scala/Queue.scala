
import scala.collection.mutable.ArrayBuffer

abstract class IntQueue {
    def get(): Int
    def put(x: Int)
}

class BasicIntQueue extends IntQueue {
    private val buf = new ArrayBuffer[Int]
    def get() = buf.remove(0)
    def put(x: Int) { buf += x }
}

trait Doubling extends IntQueue {
    abstract override def put(x: Int) { 
//        println("super.put in Doubling")
        super.put(2 * x) 
    }
}

trait Incrementing extends IntQueue {
    abstract override def put(x: Int) {
//        println("super.put in Incrementing")
        super.put(x + 1) 
    }
}

trait Filtering extends IntQueue {
    abstract override def put(x: Int) {
//        println("super.put in Filtering")
        if (x >= 0) super.put(x) 
    }
}

//val queue = new BasicIntQueue with Filtering with Incrementing
val queue = new BasicIntQueue with Incrementing with Filtering
queue.put(-1)
queue.put(0)
queue.put(1)

println("Results:")
println(queue.get())
println(queue.get())

