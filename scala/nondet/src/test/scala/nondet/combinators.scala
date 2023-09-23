package nondet

import org.scalatest.flatspec.AnyFlatSpec

package sexps {
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

    def parse(tokens: List[Elem]): Seq[Exp] = {
      expP.completeParses(tokens).toSeq
    }
  }

  class TestSExps extends AnyFlatSpec {
    import Parser._

    "An s-expression parser" should "handle integers" in {
      // 5
      assertResult(Seq(IntLiteralExp(5))) {
        parse(List(IntToken(5)))
      }
    }

    it should "handle non-nested addition" in {
      // (+ 1 2)
      assertResult(Seq(PlusExp(IntLiteralExp(1), IntLiteralExp(2)))) {
        parse(List(LeftParenToken, PlusToken, IntToken(1), IntToken(2), RightParenToken))
      }
    }

    it should "handle nested addition" in {
      // (+ (+ 1 2) (+ 3 4))
      val expected =
        PlusExp(
          PlusExp(IntLiteralExp(1), IntLiteralExp(2)),
          PlusExp(IntLiteralExp(3), IntLiteralExp(4)))
      val tokens =
        List(
          LeftParenToken,
          PlusToken,
          LeftParenToken,
          PlusToken,
          IntToken(1),
          IntToken(2),
          RightParenToken,
          LeftParenToken,
          PlusToken,
          IntToken(3),
          IntToken(4),
          RightParenToken,
          RightParenToken)
      assertResult(Seq(expected)) {
        parse(tokens)
      }
    }
  }
}

