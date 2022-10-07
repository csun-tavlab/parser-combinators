// lexer: String -> List<Token>
// parser: List<Token> -> AST

enum MyBoolean {
    case Truth
    case Falsehood
}

func returnSomething(_ param: MyBoolean) -> Int {
    switch param {
    case .Truth: return 1
    case .Falsehood: return 0
    }
}

func returnSomethingButBool(_ param: Bool) -> Int {
    if param {
        return 1
    } else {
        return 0
    }
}

enum MyEnum {
    case Foo
    case Bar(Int)
    case Baz(Int, Bool)
}

// _: external name (if underscore, it's not given an external name)
// from: internal name
func extractInt(_ from: MyEnum) -> Int {
    // intention:
    // if from is a Foo, return 0
    // if from is a Bar, extract the packaged Int and return it
    // if from is a Baz, extract the packaged Int and return it
    switch from {
    case .Foo: return 0
    case .Bar(let theInteger): return theInteger
    case .Baz(let x, _): return x
    }
}

print(extractInt(MyEnum.Foo))
print(extractInt(MyEnum.Bar(7)))
print(extractInt(MyEnum.Baz(8, true)))

