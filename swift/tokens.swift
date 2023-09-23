// lexer: String -> List<Token>

// In Java:
// public interface Token {}
// public class LeftParenToken implements Token {}
// public class IntToken implements Token {
//     public final int value;
//     public IntToken(final int value) {
//         this.value = value;
//     }
// }

// In Swift:
enum Token {
    case LeftParenToken
    case IntToken(Int)
}
