package nondet

// exp ::= INTEGER | `(` `+` exp exp `)`

sealed trait Token
case class IntToken(i: Int) extends Token
case object LeftParenToken extends Token
case object PlusToken extends Token
case object RightParenToken extends Token

sealed trait Exp
case class IntLiteralExp(i: Int) extends Exp
case class PlusExp(left: Exp, right: Exp) extends Exp

object Parser extends Combinators {
  override type Elem = Token

  lazy val intP: Parser[Int] = accept({ case IntToken(i) => i })
  lazy val expP: Parser[Exp] = {
    intP.map(IntLiteralExp.apply _) |
    (LeftParenToken ~ PlusToken ~ expP ~ expP ~ RightParenToken) ^^ { case _ ~ _ ~ e1 ~ e2 ~ _ => PlusExp(e1, e2) }
  }
}
