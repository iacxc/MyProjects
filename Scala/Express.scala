
import expr._

object Express extends Application {
    val f = new ExprFormatter

    val e1 = BinOp("*", BinOp("/", Number(1), Number(2)),
                        BinOp("+", Var("x"), Number(1)))

    def show(e: Expr) = println(f.format(e) + "\n\n")

    for (e <- Array(e1)) show(e)
}
