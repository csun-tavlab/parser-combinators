package nondet

import org.scalatest.flatspec.AnyFlatSpec

package sexps {
  // deterministic
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

    it should "fail on something syntactically invalid" in {
      // (+ 1 2
      val tokens =
        List(
          LeftParenToken,
          PlusToken,
          IntToken(1),
          IntToken(2))
      assertResult(Seq()) {
        parse(tokens)
      }
    }
  }
}

package dangling_else {
  // nondeterministic
  // derived from https://en.wikipedia.org/wiki/Ambiguous_grammar
  //
  // Note that this one was used instead of the other examples on that
  // page, because all the other examples are of left-recursive grammars.
  // Factoring them to no longer be left-recursive would make them
  // unambiguous, at least with the ways I'm used to factoring them.
  //
  // stmt ::= `if` exp `then` stmt |
  //          `if` exp `then` stmt `else` stmt
  // exp ::= INTEGER

  sealed trait Token
  case class IntToken(i: Int) extends Token
  case object IfToken extends Token
  case object ThenToken extends Token
  case object ElseToken extends Token
  case object PassToken extends Token

  sealed trait Exp
  case class IntLiteralExp(i: Int) extends Exp

  sealed trait Stmt
  case object PassStmt extends Stmt
  case class IfStmt(guard: Exp, ifTrue: Stmt, ifFalse: Option[Stmt]) extends Stmt

  object Parser extends Combinators {
    override type Elem = Token

    lazy val intP: Parser[Int] = accept({ case IntToken(i) => i })
    lazy val expP: Parser[Exp] = intP ^^ (IntLiteralExp.apply _)
    lazy val stmtP: Parser[Stmt] = {
      PassToken ^^^ PassStmt |
      (IfToken ~ expP ~ ThenToken ~ stmtP ~ opt(ElseToken ~ stmtP) ^^
        { case _ ~ e ~ _ ~ s1 ~ op => IfStmt(e, s1, op.map(_._2)) })
    }

    def parse(tokens: List[Elem]): Seq[Stmt] = {
      stmtP.completeParses(tokens).toSeq
    }
  }

  class TestDanglingElse extends AnyFlatSpec {
    import Parser._

    "A parser of if/else" should "handle pass" in {
      assertResult(Seq(PassStmt)) {
        parse(List(PassToken))
      }
    }

    it should "handle unambiguous no else" in {
      // if 5 then pass
      val tokens =
        List(IfToken, IntToken(5), ThenToken, PassToken)
      val expected = Seq(IfStmt(IntLiteralExp(5), PassStmt, None))
      assertResult(expected) {
        parse(tokens)
      }
    }

    it should "handle unambiguous with else" in {
      // if 5 then pass else pass
      val tokens =
        List(IfToken, IntToken(5), ThenToken, PassToken, ElseToken, PassToken)
      val expected = Seq(IfStmt(IntLiteralExp(5), PassStmt, Some(PassStmt)))
      assertResult(expected) {
        parse(tokens)
      }
    }

    it should "handle ambiguous if/else" in {
      // if 5 then if 6 then pass else pass
      // ==> if 5 then (if 6 then pass else pass)
      // ==> if 5 then (if 6 then pass) else pass
      val tokens =
        List(
          IfToken,
          IntToken(5),
          ThenToken,
          IfToken,
          IntToken(6),
          ThenToken,
          PassToken,
          ElseToken,
          PassToken)
      val expected =
        Seq(
          IfStmt(
            IntLiteralExp(5),
            IfStmt(
              IntLiteralExp(6),
              PassStmt,
              Some(PassStmt)),
            None),
          IfStmt(
            IntLiteralExp(5),
            IfStmt(
              IntLiteralExp(6),
              PassStmt,
              None),
            Some(PassStmt)))
      assertResult(expected) {
        parse(tokens)
      }
    }
  }
}
