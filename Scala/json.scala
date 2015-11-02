
import scala.util.parsing.combinator._

class JSON extends JavaTokenParsers {
    def obj: Parser[Map[String, Any]] =
        "{" ~> repsep(member, ",") <~ "}" ^^ (Map() ++ _)
    def arr: Parser[List[Any]] =
        "[" ~> repsep(value, ",") <~ "]"
    def member: Parser[(String, Any)] =
        stringLiteral ~ ":" ~ value ^^
            { case name ~ ":" ~ value => (name, value)}
    def value: Parser[Any] = (
        obj
        | arr
        | stringLiteral
        | floatingPointNumber ^^ (_.toDouble)
        | "null" ^^ (x => null)
        | "true" ^^ (x => true)
        | "false" ^^ (x => false)
        )

    def parse(input: String): Map[String, Any] = 
        parseAll(obj, input) match {
            case Success(e, _) => e
            case f: NoSuccess => Map("error" -> f.msg)
        }

}
