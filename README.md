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

![image](https://github.com/arnaavgoyal/new-compiler/assets/58274830/4bbd7565-e870-4b21-ac71-16ae647bf976)

Pretty awesome, right?

At this point, I have set myself up with a solid base of utility classes. Time to start with the actual compiler.

## Lexing
The `Token` class is a hugely upgraded version of the `token` struct from my last compiler. It contains its source location, string representation, and type.
C++ `std::string` is so much easier to work with than C `char *`, and source locations fix my token location woes.
It is declared in [lexer/token.h][7] and defined in [lexer/token.cpp][8].

My token types also got a massive upgrade from last time. This time, the token types are much more specific, which will make parsing significantly easier while not increasing lexing difficulty too much.
For example, I had one catch-all value for operators in the old compiler, `TYPE_OPERATOR`, but I now have a distinct enum value for every valid operator.

I also used a trick from llvm, factoring all token definitions out into a separate file, [lexer/tokendefs][9]. This makes changing language constructs very easy.
This trick also allows for language construct token strings to be common. This can lead to big memory savings with big programs.
For example, if the source code has 400 "+" tokens, I can have just one "+" string in memory, rather than 400 different strings that are all "+".

Finally, there's the lexer itself.
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

## Parsing
The `Parser` class implements a fully featured LR(1) parser.
It is declared in [parser/parser.h][12] and defined in [parser/parser.cpp][13].

The expression parsing is done via recursive descent, with a hierarchy of functions that are called in order of operation precedence.
All of the expression parsing logic is generalized into four functions: `left_assoc_bin_op`, `right_assoc_bin_op`, `parse_postfix`, and `parse_prefix`.
Every binary operation precedence function is implemented by creating a list of applicable operators and then calling the corresponding generalized `*_bin_op` function.
For example, since `+` is a left associative binary operation, it is implemented using `left_assoc_bin_op`.

All language statements such as keyword statements, function declarations, and expressions are determined in the top level `parse_stmt` function,
and then delegated to specific helper functions like `parse_decl` and `parse_expr`.

The `Parser` works hand-in-hand with the `SemanticAnalyzer` class.
Whenever a new statement is parsed and determined to be syntactically correct, the statement is passed along to be semantically checked.
From the parser's point of view, it receives opaque result objects that it passes back to the analyzer as it ascends up the recursive call stack.


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
