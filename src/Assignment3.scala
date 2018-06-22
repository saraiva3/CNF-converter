import scala.util.parsing.combinator.JavaTokenParsers

abstract class Expr
case class Const(value: Boolean) extends Expr
case class Atom(name: String) extends Expr
case class Not(expr: Expr) extends Expr
case class And(left: Expr, right: Expr) extends Expr
case class  Or(left: Expr, right: Expr) extends Expr
/* ra ra
ra ra
*/
 
class MyParser extends JavaTokenParsers {

    def expr = chainl1(factor, parseAndOr) ; 
    def term = chainl1(factor, parseAndOr) ; def parseAndOr = "/\\" ^^^ {And} | "\\/" ^^^ {Or}
    def factor: Parser[Expr] = "~" ~> factor ^^ {Not} | primary 
    def primary = "1" ^^^ {Const(true)} | "0" ^^^ {Const(false)} | ident ^^ {Atom} | "(" ~> expr <~ ")"
}

object Main extends MyParser {



    def simplify(expr: Expr): Expr = {
    
        def simplifyH(expr: Expr) = expr match {
            case Not(   Const(true))  => Const(false) 
            case Not(Const(false))    => Const(true)
            case And(p, Const(false)) => Const(false) 
            case And(Const(false), p) => Const(false)
            case And(p, Const(true))  => p            
            case And(Const(true),  p) => p
            case  Or(p, Const(false)) => p            
            case  Or(Const(false), p) => p
            case  Or(p, Const(true))  => Const(true) 
            case  Or(Const(true),  p) => Const(true)              
            case Not(Not(p)) => p                     
            case e => e }
        
        expr match {
            case Not(p)    => simplifyH(Not(simplify(p)))
            case And(p, q) => simplifyH(And(simplify(p), simplify(q)))
            case  Or(p, q) => simplifyH( Or(simplify(p), simplify(q)))
           
            case e => e }
    }

    
    
    def cnf(expr: Expr): Expr = {
    
        def distr(expr: Expr): Expr = expr match {
            case Or(And(p, q), And(r, s)) => And(And(distr(Or(p, r)), distr(Or(p, s))), 
                                                 And(distr(Or(q, r)), distr(Or(q, s))))
            case Or(p, And(q, r)) => And(distr(Or(p, q)), distr(Or(p, r)))
            case Or(And(p, q), r) => And(distr(Or(p, r)), distr(Or(q, r))) 
            case e => e }

        expr match {
           case Not( Or(p, q)) => And(cnf(Not(p)), cnf(Not(q)))
            case  Or(p, q) => distr(Or(cnf(p), cnf(q))) case And(p, q) => And(cnf(p), cnf(q)) 
            case e => e }
    } 
    def main(args: Array[String]) {
 
        var input = ""
 
        do  { input = readLine() ; if (input != null && input != "") {
            
            val output = parseAll(expr, input) match {
                case Failure(msg, _) => "failure: " + msg case Error(msg, _) => "error: " + msg
                case Success(parsed, _) => {                   
                    val simplif = simplify(parsed)
                    val simpliCnf = cnf(simplify(parsed))
                   parsed.toString + "\n" + 
                         simplif.toString() + "\n"+simpliCnf.toString()
                }
            }
            
            println(output) }
        } while (input != null && input != "")
    }
}