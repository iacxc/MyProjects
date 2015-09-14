
def makeRowSeq(row: Int, rows: Int) =
    for (col <- 1 to rows) yield {
        val prod = (row * col).toString
        val padding = " " * (4 - prod.length)
        padding + prod
    }

def makeRow(row: Int, rows: Int) = makeRowSeq(row, rows).mkString

def multiTable(rows: Int) = {
    val tableSeq = for (row <- 1 to rows) yield makeRow(row, rows)
    tableSeq.mkString("\n")
}
