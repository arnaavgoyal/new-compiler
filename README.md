# new-compiler
My second, more informed attempt at making a compiler. Written in C++, to compile my own language.

I started this project after I decided my first compiler ([here][1]) needed a complete rewrite.

## Foundations
The biggest learning from the first compiler was the importance of design, so I took a lot of time setting the foundations of my design to ensure success later on.

### Dynamic Allocation
Dynamic allocation was necessary for my design, but I did not want to fall into the same pit as last time with manual allocation and a memory leak nightmare.
To solve this, I created an allocator to efficiently allocate objects and automatically deallocate them (destructors are the greatest thing ever created).

My allocator is declared and defined in [memory/allocator.h][2].
It allocates buffers big enough to hold `buf_size` number of templated objects and adds their pointers to a vector.
Every time it runs out of space in the current buffer, it allocates a new buffer.
This approach has the benefit of grouping allocations, so that a memory allocation syscall only happens every `buf_size` allocations rather than every time a new object is allocated.

I was incredibly proud of my creation until I discovered that `std::allocator` already exists. Oh well.

### Source Management
I wanted my compiler to be able to compile multiple source files at once, rather than being locked to one.
To this end, I created a suite of source management tools for myself to use in the future.
These are all declared in [source/source.h][3] and defined in [source/source.cpp][4].

The `SourceManager` holds a list of all source file paths.
When a source file is added, it returns a corresponding `SourceID` object. This object is essentially the "key" to the right source file in the `SourceManager`.
When any part of the compiler wants access to any source file, it calls `open_source` with the `SourceID` object, and the `SourceManager` will return a new input stream for the corresponding file.
This setup provides a central place to access source files, and will hopefully make juggling multiple source files much easier in the future.
The intended caller flow looks like this:
```cpp
// in compilation driver
std::string path = SRCPATH;
SourceID src_id = SourceManager::add_source(path);

// in compiler component
std::ifstream *src = SourceManager::open_source(src_id);
```

I also made `SourceLocation`, which is a direct answer to the problem I had with the first compiler of bad token location data.
This class holds all the data I could ever want about a token's location in the source, and it will be a part of the token class.
This way, every token will have its location as part of it, making refactoring and error tracing much easier.

### Error Handling
After the last compiler, I wanted this one to have proper error handling and nice, descriptive error messages.

The `ErrorHandler` class is my solution to this.
It is declared in [error/error.h][5] and defined in [error/error.cpp][6].

The main attraction of the class is the `handle` function. Given an error type, source location, and error string, it pretty-prints the error to the console.
It has all the bells and whistles of a production compiler: line numbers, context, underlining, and color highlighting.
This is what an error printed using this function looks like:

![image](https://github.com/arnaavgoyal/new-compiler/assets/58274830/8a1c493b-22b6-4efb-aa61-2fb4f3aea3b7)

Pretty awesome, right?

At this point, I have set myself up with a solid base of utility classes. Time to start with the actual compiler.

## Lexing

### Tokens
The `Token` class is a hugely upgraded version of the `token` struct from my last compiler. It contains its source location, string representation, and type.
C++ `std::string` is so much easier to work with than C `char *`, and source locations fix my token location woes.
It is declared in [lexer/token.h][7] and defined in [lexer/token.cpp][8].

My token types also got a massive upgrade from last time. This time, the token types are much more specific, which will make parsing significantly easier while not increasing lexing difficulty too much.
For example, I had one catch-all value for operators in the old compiler, `TYPE_OPERATOR`, but I now have a distinct enum value for every valid operator.

I also used a trick from llvm, factoring all token definitions out into a separate file, [lexer/tokendefs][9]. This makes changing language constructs very easy.
This trick also allows for language construct token strings to be common. This can lead to big memory savings with big programs.
For example, if the source code has 400 "+" tokens, I can have just one "+" string in memory, rather than 400 different strings that are all "+".

### The Lexer
The `Lexer` class encapsulates all lexing logic, and outputs lexed tokens to the rest of the program.
It is declared in [lexer/lexer.h][10] and defined in [lexer/lexer.cpp][11].

The main functionality is contained within `lex_token`.
This function is essentially a massive switch-case statement that compares against every legal character in the language.
If it finds a match, it continues matching based on possible language constructs until deviation or complete match.
Complete matches are lexed as the corresponding type (such as `token::plus`), while unmatched tokens are lexed as `token::unknown`.

A big change from my last lexer is that this lexer lexes keyword tokens differently from identifier tokens. Not doing so last time caused me lots of problems, so I made sure to do it right this time.

`lex_token` is exposed as `lex`, and a typical caller flow looks like this:
```cpp
Token tk;
lex(tk);
// use lexed token
```

The lexer was pretty quick this time. The parser and analyzer, however, is a different story.

## Syntactic and Semantic Analyzation

### The AST
The ultimate goal of my parser and analyzer is to produce a semantically correct AST, which I can use later in code generation.
To this end, I created the ast module to house all of the ast definitions and utilities.

It contains the `ASTNode` class, which contains all of the information I could ever need about the statements it represents, as well as a list of children nodes.
This class is what the parser and analyzer use to construct the AST.

It also contains `ASTNode::print()`, which is by far the most important function in the module, and my favorite.
The `print()` function pretty-prints the AST, which is absolutely invaluable when it comes to debugging the parser and analyzer.
All of the AST representations on this page were printed using this function.

### The Parser and Semantic Analyzer

The `Parser` class implements a fully featured LL(1) parser.
It is declared in [parser/parser.h][12] and defined in [parser/parser.cpp][13].

The expression parsing is done via recursive descent, with a hierarchy of functions that are called in order of operation precedence.
All of the expression parsing logic is generalized into four functions: `left_assoc_bin_op`, `right_assoc_bin_op`, `parse_postfix`, and `parse_prefix`.
Every binary operation precedence function is implemented by creating a list of applicable operators and then calling the corresponding generalized `*_bin_op` function.
For example, since `+` is a left associative binary operation, it is implemented using `left_assoc_bin_op`.

All language statements such as keyword statements, function declarations, and expressions are determined in the top level `parse_stmt` function,
and then delegated to specific helper functions like `parse_decl` and `parse_expr`.

The `SemanticAnalyzer` class takes parsed statements and performs all required semantic checks upon them.
Type checking, use before declaration, redeclaration, parameter mismatch, etc etc are all checked here.
It is declared in [analyzer/analyzer.h][14] and defined in [analyzer/analyzer.cpp][15]

All of this so far has been consistent since the conception of these modules.
However, due to philosophy changes and design constraints, the rest has been changed too many times to count.

First, I wanted the parser to be wholly independent of the analyzer, so I could fulfill my idealistic dream of perfect encapsulation.
Unfortunately, this was impossible.
A pillar of this project is and always has been that I want OOP and type aliasing.
The existence of these features means two things:
1. There must be a flow of semantic information back to the parser as typenames are declared to ensure correct parsing of types, and
2. As a direct result, semantic analysis must (at least partially) occur alongside parsing to create that semantic information.
So, no encapsulation, and no separate parsing and analysis passes. They must occur simultaneously and share information.

With these constraints in mind, I came up with another system.
This time, the parser hands parsed information to the analyzer at every step.
This way, the analyzer is able to keep all of the semantic information up to date at all times.
For example, this is how `return y && z * 3` is parsed:
1. `z * 3` is parsed by `Parser::parse_multiplicative()`, then analyzed by `SemanticAnalyzer::analyze_binary_op()`
2. next, `y + (z * 3)` is parsed by `Parser::parse_additive()`, and analyzed via `SemanticAnalyzer::analyze_binary_op()`
3. finally, `return (y + (z * 3))` is parsed by `Parser::parse_stmt()` and analyzed by `SemanticAnalyzer::analyze_return_stmt()`

This worked much better than the original idea.

However, I still wanted encapsulation.
To me, AST construction, scope stack maintenance, and symbol tables are semantic logic, so I tried to put all of that inside the `SemanticAnalyzer` class.
Doing this turned out to be a massive headache.
Since the parser uses recursive descent to parse input tokens, it very nicely models the AST, and it has all necessary context about current declaration state and scope.
The analyzer, on the other hand, knows about nothing other than the information passed to it by the parser.
This makes it impossible to keep AST construction and scope logic solely in the analyzer.

One workaround I attempted was opaque objects.
Every analyzer function would return an `AnalyzerResult` object, which contained constructed AST nodes:

```cpp
template <typename T>
class AnalyzedGeneric {
    friend class SemanticAnalyzer;
private:
    T contents;
    ...
};
typedef AnalyzedGeneric<ASTNode *> AnalyzerResult;
typedef AnalyzedGeneric<ASTNode *> AnalyzedStmt;
typedef AnalyzedGeneric<ASTNode *> AnalyzedExpr;
typedef AnalyzedGeneric<Type const *> AnalyzedType;
```

To the parser, these objects were opaque, and it would simply pass them to other analyzer functions.
This allowed for elegant AST construction, as the analyzer would receive the `AnalyzerResult` objects in line with the AST construction (because the parser would pass them in accordance with it's ascent up the recursion stack).
Unfortunately, at global scope, the opaque objects are left to the parser, which has no idea what to do with them (due to encapsulation).
This makes the final AST impossible to construct.

After all of this going nowhere, I decided I would rather have results over beautiful code, so I moved AST construction back to the parser.

It was at this point that I also changed my symbol table.
Previously, I was using a LeBlanc-Cook symbol table, which is essentially one massive symbol table that handles scoping with supplemental scope information.
This seemed to be cumbersome, and in the interest of future-proofing my symbol table design, I swapped to per-scope symbol tables.
During this change, I moved the broad scoping logic back to the parser, as it made no sense to keep it in the analyzer any longer since it now closely mirrored the AST logic.
However, the scope objects are still mostly opaque so as to keep the analyzer in charge of maintaining the symbol tables.

This is what a statement parsing flow currently looks like (this is for parsing return):

```cpp
// cache start loc
loc_cache = tk.get_src_loc();

// make curr token the start of expr
consume();

// parse expr
AnalyzedExpr temp = parse_expr();

// analyze return stmt
res = (ASTNode *)analyzer.analyze_return_stmt(temp).contents;
```

As you can see, there are still result objects being used.
This is because the parser and analyzer are in a transition state as I refactor them using my new strategy.
I am also in the process of making new additions to the language such as loops and if-else.

As an example of the current capabilities, this code:

```cpp
using i3 = i32;

decl (i3, i32) *()i32
foo (first, second) {
    decl i32 i;
    decl i64 j;
    decl i64 first;
    foo(first, second);
    decl (i3)i3 food (first) {
        decl i32 bar;
        foo(first, bar);
    };
    return i * j + bar;
};

decl () *i3
bar () {
    return first;
};
```

is synthesized into this AST:

![image](https://github.com/arnaavgoyal/new-compiler/assets/58274830/c93e6507-fdf3-45f7-b1a6-2cf66ee4f912)

with these semantic errors:

![image](https://github.com/arnaavgoyal/new-compiler/assets/58274830/28194a16-3ce8-4e11-82a1-c8bcd24601fa)


[1]:  https://github.com/arnaavgoyal/compiler
[2]:  ../main/inc/memory/allocator.h
[3]:  ../main/inc/source/source.h
[4]:  ../main/src/source/source.h
[5]:  ../main/inc/error/error.h
[6]:  ../main/src/error/error.h
[7]:  ../main/inc/lexer/token.h
[8]:  ../main/src/lexer/token.cpp
[9]:  ../main/inc/lexer/tokendefs
[10]: ../main/inc/lexer/lexer.h
[11]: ../main/src/lexer/token.cpp
[12]: ../main/inc/parser/parser.h
[13]: ../main/src/parser/parser.cpp
[14]: ../main/inc/analyzer/analyzer.h
[15]: ../main/src/analyzer/analyzer.cpp
