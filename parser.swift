// parser: List<Token> -> AST

// binop ::= `+` | `&&`
// expression ::= INTEGER | `true` | `false` | expression binop expression

public class SourceSpan {
    final int startingLine;
    final int startingColumn;
    final int endingLine;
    final int endingColumn;
}

public abstract class Expression {
    public final SourceSpan sourceSpan;
    public Expression(final SourceSpan sourceSpan) {
        this.sourceSpan = sourceSpan;
    }
    public SourceSpan getSourceSpan() {
        return sourceSpan;
    }
}
public class IntegerLiteralExpression extends Expression {
    public final int value;
    public IntegerLiteralExpression(final SourceSpan sourceSpan, final int value) {
        super(sourceSpan);
        this.value = value;
    }
}
// public class TrueExpression implements Expression {}
// public class FalseExpression implements Expression {}
// public interface Binop {}
// public class PlusBinop implements Binop {}
// public class AndBinop implements Binop {}
// public class BinopExpression implements Expression {
//     public final Expression left;
//     public final Binop binop;
//     public final Expression right;
//     public BinopExpression(final Expression left, final Binop binop, final Expression right) {
//         this.left = left;
//         this.binop = binop;
//         this.right = right;
//     }
// }
// // plus expression with leftExp and rightExp
// // new BinopExpression(leftExp, new PlusBinop(), rightExp)

enum Binop {
    case Plus
    case And
}

indirect enum Expression {
    case IntegerLiteralExpression(Int)
    case TrueExpression
    case FalseExpression
    case BinopExpression(Expression, Binop, Expression)
}

// let withInformation: Sourced<Expression> = Sourced(SourceSpan(start, end), Expression.IntegerLiteralExpression(4))
// let alsoWithInformation: Sourced<Expression> = Sourced(SourceSpan(start, end),
//                                                        Expression.BinopExpression(Sourced(SourceSpan(leftStart, leftEnd),
//                                                                                           ...,
//                                                                                           ...))

// recursive-descent parsing
// Swift: parser combinators
