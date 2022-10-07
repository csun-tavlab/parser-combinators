// parser: List<Token> -> AST

// binop ::= `+` | `&&`
// expression ::= INTEGER | `true` | `false` | expression binop expression

// no longer left-recursive:
// primary ::= INTEGER | `true` | `false` | `(` expression `)`
// plus-expression ::= primary | primary (`+` plus-expression)*
// and-expression ::= plus-expression | plus-expression (`&&` and-expression)*
// expression ::= and-expression

enum Token {
    case PlusToken
    case AndToken
    case IntegerToken(Int)
    case TrueToken
    case FalseToken
}

enum Binop {
    case PlusBinop
    case AndBinop
}

indirect enum Expression {
    case IntegerLiteralExpression(Int)
    case BooleanLiteralExpression(Bool)
    case BinopExpression(Expression, Binop, Expression)
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
}

enum ParseResult<A> {
    case Success(A, List<Token>)
    case Failure(String)
}

typealias Parser<A> = (List<Token>) -> ParseResult<A>;

// func tokenP(_ expected: Token) -> (List<Token>) -> ParseResult<A> {
func tokenP(_ expected: Token) -> Parser<Token> {
    return { inputTokens in
        switch inputTokens {
        case .Cons(let head, let tail):
            if head == expected {
                return ParseResult.Success(head, tail)
            } else {
                return ParseResult.Failure("Expected " + expected + "; " + "Received: " + head)
            }
        case .Nil:
            return ParseResult.Failure("Expected " + expected + ", but ran out of tokens")
        }
    }
}

// func andP<A, B>(_ first: (List<Token>) -> ParseResult<A>,
//                 _ second: (List<Token>) -> ParseResult<B>) ->
//   (List<Token>) -> ParseResult<(A, B)>
func andP<A, B>(_ first: Parser<A>, _ second: Parser<B>) -> Parser<(A, B)> {
    return { inputTokens in
        switch first(inputTokens) {
        case .Failure(let s): return .Failure(s)
        case .Success(let a, let rest):
            switch second(rest) {
            case .Failure(let s): return .Failure(s)
            case .Success(let b, let finalRest):
                return .Success((a, b), finalRest)
            }
        }
    }
}

func orP<A>(_ first: Parser<A>, _ second: Parser<A>) -> Parser<A> {
    return { inputTokens in
        switch first(inputTokens) {
        case .Success(let a, let rest): return .Success(a, rest)
        case .Failure(_): return second(inputTokens)
        }
    }
}

func mapP<A, B>(_ parser: Parser<A>, _ f: (A) -> B) -> Parser<B> {
    return { inputTokens in
        switch parser(inputTokens) {
        case .Success(let a, let rest): return .Success(f(a), rest)
        case .Failure(let s): return .Failure(s)
        }
    }
}

func plusParser() -> Parser<Binop> {
    return mapP(tokenP(Token.PlusToken), { token in Binop.PlusBinop })
}

func andParser() -> Parser<Binop> {
    return mapP(tokenP(Token.AndToken), { token in Binop.AndBinop })
}

func binopParser() -> Parser<Binop> {
    return orP(plusParser(), andParser())
}

// primary | primary (`+` plus-expression)*
// orP(primaryP(), andP(primaryP(), starP(andP(plusP(), plusExpressionP()))))
