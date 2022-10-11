// parser: List<Token> -> AST

// binop ::= `+` | `&&`
// expression ::= INTEGER | `true` | `false` | expression binop expression

// no longer left-recursive:
// primary ::= INTEGER | `true` | `false` | `(` expression `)`
// plus-expression ::= primary (`+` primary)*
// and-expression ::= plus-expression (`&&` plus-expression)*
// expression ::= and-expression

public enum Token : CustomStringConvertible, Equatable {
    case PlusToken
    case AndToken
    case IntegerToken(Int)
    case TrueToken
    case FalseToken
    case LeftParenToken
    case RightParenToken

    public var description: String {
        switch self {
        case .PlusToken: return "+"
        case .AndToken: return "&&"
        case .IntegerToken(let value): return String(value)
        case .TrueToken: return "true"
        case .FalseToken: return "false"
        case .LeftParenToken: return "("
        case .RightParenToken: return ")"
        }
    }
}

public func ==(lhs: Token, rhs: Token) -> Bool {
    switch (lhs, rhs) {
    case (.PlusToken, .PlusToken),
         (.AndToken, .AndToken),
         (.TrueToken, .TrueToken),
         (.FalseToken, .FalseToken),
         (.LeftParenToken, .LeftParenToken),
         (.RightParenToken, .RightParenToken): return true
    case (.IntegerToken(let x), .IntegerToken(let y)) where x == y: return true
    case _: return false
    }
}

enum Binop : CustomStringConvertible {
    case PlusBinop
    case AndBinop

    public var description: String {
        switch self {
        case .PlusBinop: return "+"
        case .AndBinop: return "&&"
        }
    }
}

indirect enum Expression : CustomStringConvertible {
    case IntegerLiteralExpression(Int)
    case BooleanLiteralExpression(Bool)
    case BinopExpression(Expression, Binop, Expression)

    public var description: String {
        switch self {
        case .IntegerLiteralExpression(let value): return String(value)
        case .BooleanLiteralExpression(let value): return String(value)
        case .BinopExpression(let lhs, let binop, let rhs):
            return "(\(lhs), \(binop), \(rhs))"
        }
    }
}

// more general (where A is a generic type): List<Token> -> A

// handles error: List<Token> -> ParseResultWithOnlyError<A>

enum ParseResultWithOnlyError<A> {
    case Success(A)
    case Failure(String)
}

// handles partial information: List<Token> -> ParseResult<A>

// [1, 2, 3] = List.Cons(1, List.Cons(2, List.Cons(3, List.Nil)))

indirect enum List<A> {
    case Cons(A, List<A>)
    case Nil

    func foldLeft<B>(_ accum: B, _ f: (B, A) -> B) -> B {
        switch self {
        case .Nil: return accum
        case .Cons(let head, let tail):
            return tail.foldLeft(f(accum, head), f)
        }
    }
    
    // Cons(1, Cons(2, Cons(3, Nil)))
    // Cons(3, Cons(2, Cons(1, Nil)))
    func reverse() -> List<A> {
        func helper(input: List<A>, accum: List<A>) -> List<A> {
            switch input {
            case .Nil: return accum
            case .Cons(let head, let tail):
                return helper(input: tail, accum: .Cons(head, accum))
            }
        }
        return helper(input: self, accum: .Nil)
    }
}

enum ParseResult<A> {
    case Success(A, List<Token>)
    case Failure(String)
}

typealias Parser<A> = (List<Token>) -> ParseResult<A>;

func intP() -> Parser<Int> {
    return { inputTokens in
        switch inputTokens {
        case .Cons(.IntegerToken(let value), let tail):
            return ParseResult.Success(value, tail)
        case .Cons(let head, _):
            return ParseResult.Failure("Expected integer token; received: \(head)")
        case .Nil:
            return ParseResult.Failure("Expected integer token, but ran out of tokens")
        }
    }
}

// func tokenP(_ expected: Token) -> (List<Token>) -> ParseResult<A> {
func tokenP(_ expected: Token) -> Parser<Token> {
    return { inputTokens in
        switch inputTokens {
        case .Cons(let head, let tail):
            if head == expected {
                return ParseResult.Success(head, tail)
            } else {
                return ParseResult.Failure("Expected \(expected); Received: \(head)")
            }
        case .Nil:
            return ParseResult.Failure("Expected \(expected), but ran out of tokens")
        }
    }
}

// func andP<A, B>(_ first: (List<Token>) -> ParseResult<A>,
//                 _ second: (List<Token>) -> ParseResult<B>) ->
//   (List<Token>) -> ParseResult<(A, B)>
func andP<A, B>(_ first: @escaping @autoclosure () -> Parser<A>,
                _ second: @escaping @autoclosure () -> Parser<B>) -> Parser<(A, B)> {
    return { inputTokens in
        switch first()(inputTokens) {
        case .Failure(let s): return .Failure(s)
        case .Success(let a, let rest):
            switch second()(rest) {
            case .Failure(let s): return .Failure(s)
            case .Success(let b, let finalRest):
                return .Success((a, b), finalRest)
            }
        }
    }
}

func orP<A>(_ first: @escaping @autoclosure () -> Parser<A>,
            _ second: @escaping @autoclosure () -> Parser<A>) -> Parser<A> {
    return { inputTokens in
        switch first()(inputTokens) {
        case .Success(let a, let rest): return .Success(a, rest)
        case .Failure(_): return second()(inputTokens)
        }
    }
}

func mapP<A, B>(_ parser: @escaping @autoclosure () -> Parser<A>,
                _ f: @escaping (A) -> B) -> Parser<B> {
    return { inputTokens in
        switch parser()(inputTokens) {
        case .Success(let a, let rest): return .Success(f(a), rest)
        case .Failure(let s): return .Failure(s)
        }
    }
}

func starP<A>(_ parser: @escaping @autoclosure () -> Parser<A>) -> Parser<List<A>> {
    func starPHelper(parser: Parser<A>,
                     tokens: List<Token>,
                     accum: List<A>) -> (List<A>, List<Token>) {
        switch parser(tokens) {
        case .Failure(_):
            return (accum.reverse(), tokens)
        case .Success(let a, let rest):
            return starPHelper(parser: parser,
                               tokens: rest,
                               accum: .Cons(a, accum))
        }
    }
    
    return { inputTokens in
        let (list, tokens) = starPHelper(parser: parser(),
                                         tokens: inputTokens,
                                         accum: List.Nil)
        return .Success(list, tokens)
    }
}

func inParens<A>(_ parser: @escaping @autoclosure () -> Parser<A>) -> Parser<A> {
    // def inParens[A](parser: => Parser[A]): Parser[A] = {
    //   leftParen ~ parser ~ rightParen ^^ { case _ ~ a ~ _ => a }
    // }
    let withParens: Parser<(Token, (A, Token))> =
      andP(tokenP(Token.LeftParenToken),
           andP(parser(),
                tokenP(Token.RightParenToken)))
    return mapP(withParens, { pair in pair.1.0 })
}

// some type A
// a thunk wrapping A: () -> A
// primary ::= INTEGER | `true` | `false` | `(` expression `)`
func primaryP() -> Parser<Expression> {
    orP(mapP(intP(), { value in Expression.IntegerLiteralExpression(value) }),
        orP(mapP(tokenP(Token.TrueToken), { _ in Expression.BooleanLiteralExpression(true) }),
            orP(mapP(tokenP(Token.FalseToken), { _ in Expression.BooleanLiteralExpression(false) }),
                inParens(expressionP()))))
}

func levelP(expParser: @escaping @autoclosure () -> Parser<Expression>,
            tokenParser: @escaping @autoclosure () -> Parser<Token>,
            combiner: @escaping (Expression, Expression) -> Expression) -> Parser<Expression> {
    let withStar: Parser<(Expression, List<(Token, Expression)>)> =
      andP(expParser(), // Parser<(Expression, List<(Token, Expression)>)>
           starP(andP(tokenParser(), expParser())))
    // 1 + 2 + 3 + 4
    // (1, [(+, 2), (+, 3), (+, 4)])
    // BinopExpression(BinopExpression(BinopExpression(1, +, 2), +, 3), +, 4)
    return mapP(withStar, { pair in
                              pair.1.foldLeft(pair.0, { (accum, curPair) in
                                                          combiner(accum, curPair.1)
                                                      })
                          })
}
    
// plus-expression ::= primary (`+` primary)*
func plusExpressionP() -> Parser<Expression> {
    return levelP(expParser: primaryP(),
                  tokenParser: tokenP(Token.PlusToken),
                  combiner: { (lhs, rhs) in
                      Expression.BinopExpression(lhs, Binop.PlusBinop, rhs)
                  })
}

// and-expression ::= plus-expression (`&&` plus-expression)
func andExpressionP() -> Parser<Expression> {
    return levelP(expParser: plusExpressionP(),
                  tokenParser: tokenP(Token.AndToken),
                  combiner: { (lhs, rhs) in
                      Expression.BinopExpression(lhs, Binop.AndBinop, rhs)
                  })
}

// expression ::= and-expression
func expressionP() -> Parser<Expression> {
    return andExpressionP()
}

// 1 + 2 + 3
let tokens: List<Token> =
  .Cons(.IntegerToken(1),
        .Cons(.PlusToken,
              .Cons(.IntegerToken(2),
                    .Cons(.PlusToken,
                          .Cons(.IntegerToken(3),
                                .Nil)))))

switch expressionP()(tokens) {
case .Success(let exp, .Nil):
    print(exp.description)
case .Success(_, .Cons(_, _)):
    print("Extra tokens at end")
case .Failure(let error):
    print(error)
}

