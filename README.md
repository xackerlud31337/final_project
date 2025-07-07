# MyLang - A Concurrent Programming Language Compiler

A compiler for a custom programming language that targets the Sprockell virtual machine, featuring support for concurrent programming with fork-join parallelism and shared memory synchronization.

## Features

### Language Constructs
- **Variables**: Integer and boolean scalar variables
- **Arrays**: String literals and integer arrays
- **Control Flow**: If-else statements, while loops, block scoping
- **Expressions**: Arithmetic (`+`, `-`, `*`), comparison (`==`, `!=`, `<`, `<=`, `>`, `>=`), logical (`&&`, `||`, `!`)
- **I/O**: Print statements for numbers and characters
- **Concurrency**: Fork-join parallelism with shared memory
- **Synchronization**: Lock statements for critical sections

### Example Program
```
int x = 5;
int[] arr = [1, 2, 3];
string msg = "Hello";

if (x > 0) {
    print x;
}

fork {
    lock critical {
        x = x + 1;
        print x;
    }
} join {
    lock critical {
        x = x * 2;
        print x;
    }
}
```

## Prerequisites

- **Haskell Stack** - [Install Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
- **GHC 8.10.7** (managed by Stack)
- **Sprockell** - Virtual machine for execution (included as dependency)

## Installation

1. **Clone the repository**
   ```bash
   git clone https://github.com/xackerlud31337/final_project.git
   cd final_project/stack-my-lang
   ```

2. **Build the project**
   ```bash
   stack build
   ```

3. **Run tests**
   ```bash
   stack test
   ```

## Usage

### Compile and run a program
```bash
stack run < input.mylang
```

### Interactive mode
```bash
stack ghci
*Main> compileSource "int x = 42; print x;"
```

## Project Structure

```
stack-my-lang/
├── src/
│   ├── MyParser.hs      # Parser for MyLang syntax
│   └── MyCodeGen.hs     # Code generator targeting Sprockell
├── app/
│   └── Main.hs          # Main executable entry point
├── test/
│   └── Spec.hs          # Test suite
├── package.yaml         # Stack project configuration
└── stack.yaml           # Stack resolver configuration
```

## Language Grammar

```
Program     ::= Statement*
Statement   ::= Declaration | Assignment | Print | Block | If | While | ForkJoin | Lock
Declaration ::= Type Identifier ['=' Expression] ';'
Type        ::= 'int' | 'bool' | 'string' | Type'[]'
Assignment  ::= Identifier '=' Expression ';'
Print       ::= 'print' Expression ';'
Block       ::= '{' Statement* '}'
If          ::= 'if' '(' Expression ')' Statement ['else' Statement]
While       ::= 'while' '(' Expression ')' Statement
ForkJoin    ::= 'fork' Block+ 'join'
Lock        ::= 'lock' Identifier Block
```

## Technical Details

### Compiler Pipeline
1. **Lexical Analysis** - Tokenizes source code
2. **Parsing** - Builds Abstract Syntax Tree (AST)
3. **Code Generation** - Translates AST to Sprockell instructions
4. **Execution** - Runs on Sprockell virtual machine

### Concurrency Model
- **Fork-Join**: Create parallel threads that synchronize at join points
- **Shared Memory**: All threads share a common memory space
- **Locks**: Named critical sections for mutual exclusion

### Memory Management
- **Scalar Variables**: Single memory slot allocation
- **Arrays**: Contiguous memory allocation with base address
- **Scoping**: Stack-based environment with block scoping

## Dependencies

- `base >= 4.7 && < 5` - Standard Haskell library
- `parsec >= 3.1 && < 3.2` - Parser combinator library
- `sprockell >= 2022.0` - Target virtual machine
- `hspec >= 2.8 && < 2.9` - Testing framework
- `QuickCheck >= 2.14 && < 2.15` - Property-based testing

## Development

### Adding New Features
1. Update grammar in `MyParser.hs`
2. Extend AST data types
3. Implement code generation in `MyCodeGen.hs`
4. Add tests in `test/Spec.hs`

### Testing
```bash
# Run all tests
stack test

# Run with coverage
stack test --coverage

# Run specific test
stack test --test-arguments="--match pattern"
```

## Known Limitations

- No function definitions (only built-in operations)
- Limited error reporting
- No type checking (runtime errors possible)
- Array bounds not checked at compile time

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## License

This project is part of an academic assignment. Please respect academic integrity policies.

## Authors

- **xackerlud31337** - Initial implementation

## Acknowledgments

- Sprockell virtual machine by Utrecht University
- Haskell Parsec library