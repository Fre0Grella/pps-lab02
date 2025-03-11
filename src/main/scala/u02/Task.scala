package u02

import scala.annotation.tailrec

object Task extends App :
  val f: Int => String =
    case n if n >= 0 => "positive"
    case _ => "negative"

  def fPos(n: Int): String = n match
   case n if n >= 0 => "positive"
   case _ => "negative"

  val neg = (fun: String => Boolean) => (string: String) => !fun(string)
  def fNeg(fun: String => Boolean): String => Boolean = (string: String) => !fun(string)

  val empty: String => Boolean = _ == "" // predicate on strings

  def negGen[X](fun: X => Boolean): X => Boolean = (x: X) => !fun(x)

  val positive: Int => Boolean = _ >= 0

  val p1: Double => Double => Double => Boolean = x => y => z => x <= y && y == z
  val p2 = (x: Double,y: Double,z: Double) => x <= y && y == z
  def p3(x: Double)(y: Double)(z: Double): Boolean = x <= y && y == z
  def p4(x: Double, y: Double, z: Double): Boolean = x <= y && y == z

  //The constraint is that the input of f need to be of the same type of the output of g
  def compose[X,Y,Z](f: X => Y, g: Z => X): Z => Y = (z: Z) => f(g(z))

  val boolToInt: Boolean => Int =
    case true => 0
    case false => 1

  def composeThree[A,B,C,D](f: C => D, g: B => C, h: A => B): A => D = (a: A) => compose(f,compose(g,h))(a)

  //Task 3: Recursion

  def power(base: Double, exponent: Int): Double = exponent match
    case 1 => base
    case n if n > 1 => base * power(base,n-1)

  def powerTail(base: Double, exponent: Int): Double =
    @tailrec
    def _powerTail(base: Double, exponent: Int, res: Double): Double = exponent match
      case 0 => res
      case n if n > 0 => _powerTail(base, n - 1, res*base)
    _powerTail(base,exponent,1)

  def reverseNumber(n: Int): Int =
    @tailrec
    def _reverseNumber(n: Int, res: Int): Int =  n match
      case 0 => res
      case n if n != 0 => _reverseNumber(n/10,res*10 + n%10)
    _reverseNumber(n,0)

  enum Expr:
    case Literal(x: Int)
    case Add(x: Expr, y: Expr)
    case Multiply(x: Expr, y: Expr)

  object Expr:
    def evaluate(expr: Expr): Int = expr match
      case Expr.Literal(n) => n
      case Expr.Add(x,y) => evaluate(x) + evaluate(y)
      case Expr.Multiply(x,y) => evaluate(x) * evaluate(y)

    def show(expr: Expr): String = expr match
      case Expr.Literal(n) => n.toString
      case Expr.Add(x, y) => "("+show(x)+"+"+show(y)+")"
      case Expr.Multiply(x, y) => show(x)+"*"+show(y)
