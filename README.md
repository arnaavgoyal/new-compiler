# new-compiler
My second, more informed attempt at making a compiler. Written in C++, to compile my own language.

I started this project after I decided my first compiler ([here][1]) needed a complete rewrite.

## Frontend

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

### The Abstract Syntax Tree
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
ASTNode *temp = parse_expr();

// analyze return stmt
res = analyzer.analyze_return_stmt(temp);
```

Once parsing is finished, the compiler can progress onwards.

## Middle-End
I wanted my compiler to be capable of doing everything that the likes of Clang and GCC can, and that includes optimization.
To this end, I created a middle-end complete with a custom IR, within which I can add any kind of analysis or transformation I like.


### IR
My IR is heavily inspired by (in my opinion) the king of optimizing compilers: LLVM.
It is in static single-assignment form, like LLVM IR and GCC gimple, for all the same reasons as them:
* Trivial analysis of the control flow graph (since the IR *is* the cfg),
* Explicit def-use chains (since the IR *is also* the dataflow graph), which makes many optimizations significantly easier to implement, and
* Semantics that are much closer to machine code, allowing for more potential optimization

A lot of design decisions in the IR were influenced directly by LLVM IR, such as:
* A deeply nested class hierarchy (I wanted to do things "right" for once),
* Iterator-based data structure traversal, and
* Many IR instructions and semantics
* Uniquing of IR constructs like constants

"If it ain't broke, don't fix it"

Developing the IR led me into the depths of LLVM and GCC to understand how they used IR and what I should consider with mine.
I developed a very deep understanding of how LLVM itself works, which not only helped with this compiler but also with my understanding of modern compiler construction and compilation pipelines.

### Translation
To translate the AST to IR, I created `ASTTranslator`, whose only purpose is to perform its namesake.
I like LLVM's philosophy of isolating SSA logic to the middle-end, so the translation step lowers variables via stack reads and writes.
This is inefficient, but it will be fixed in IR.

For example, this code:

```go
def (*i8) i32
foo(str) {
  return 0;
}

def (i32, **i8) i32
main(argc, argv) {
  var i32 val = 0;
  while (val) {
    val = 33;
    if (val) {
        val = 1;
    }
    else {
        val = 400;
    }
  }
  if (argc) {
      val = foo(*argv);
  }
  return val;
}
```

is translated to this IR:

```
i32 foo (ptr str) {
entry:
  ptr %str.addr = salloc ptr
  write ptr str, ptr %str.addr
  return i32 0
}
i32 main (i32 argc, ptr argv) {
entry:
  ptr %val = salloc i32
  ptr %argv.addr = salloc ptr
  ptr %argc.addr = salloc i32
  write i32 argc, ptr %argc.addr
  write ptr argv, ptr %argv.addr
  write i32 0, ptr %val
  branch block loopcond
loopcond:
  i32 %tmp = read i32, ptr %val
  i1 %tmp1 = icmp neq, i32 %tmp, i32 0
  branch block loopbody, i1 %tmp1, block loopend
loopbody:
  write i32 33, ptr %val
  i32 %tmp2 = read i32, ptr %val
  i1 %tmp3 = icmp neq, i32 %tmp2, i32 0
  branch block ifthen, i1 %tmp3, block ifelse
ifthen:
  write i32 1, ptr %val
  branch block ifdone
ifelse:
  write i32 400, ptr %val
  branch block ifdone
ifdone:
  branch block loopcond
loopend:
  i32 %tmp4 = read i32, ptr %argc.addr
  i1 %tmp5 = icmp neq, i32 %tmp4, i32 0
  branch block ifthen1, i1 %tmp5, block ifdone1
ifthen1:
  ptr %tmp6 = read ptr, ptr %argv.addr
  ptr %tmp7 = read ptr, ptr %tmp6
  i32 %tmp8 = call i32 foo, ptr %tmp7
  write i32 %tmp8, ptr %val
  branch block ifdone1
ifdone1:
  i32 %tmp9 = read i32, ptr %val
  return i32 %tmp9
}
```

### Passes
Again, I absolutely love LLVM's approach to analysis and optimization, and at the core of its infrastructure are Passes.
Everything in LLVM is a pass, and I wanted to approach it the same way.
As my first foray into creating an IR pass, I decided to make my own mem2reg - an LLVM transformation pass that promotes stack variables into SSA registers.
(This is how the variable translation issue is fixed, and is a common paradigm when writing frontends to LLVM)

#### Stackpromote
To create stackpromote (my mem2reg) I needed to use almost every capability afforded by my shiny new IR.
* First, it determines elibility for promotion by traversing the def-use chains for every `salloc` (my equivalent to LLVM's `alloca`)
  * If a pointer returned from `salloc` is read from (`read`) or written to (`write`), it is eligible
  * If any other instruction uses the pointer, it is not eligible (the pointer value itself must exist, so the stack allocation cannot be removed)
* Second, for each eligible `salloc`, it traverses the cfg to calculate a special dominator tree called a dj-graph [^1]
* Third, it computes the iterated dominance frontier (idf) of each `salloc` based on its dj-graph [^1]
  * Simply, the idf is the set of basic blocks in which to insert phi-nodes for optimal phi-node placement
* Fourth, it replaces all uses of the `salloc` value with a newly made SSA register
* Fifth, it places phi-nodes based on the idf, removes all relevant reads and writes, and finally removes the `salloc` itself
[^1]: https://dl.acm.org/doi/pdf/10.1145/199448.199464

And there you go! Perfect SSA code.

Here is an example pass through stackpromote:
* initial code:

```go
def (*i8) i32
foo(str) {
  return 0;
}

def (i32, **i8) i32
main(argc, argv) {
  var i32 val = 0;
  while (val) {
    val = 33;
    if (val) {
        val = 1;
    }
    else {
        val = 400;
    }
  }
  if (argc) {
      val = foo(*argv);
  }
  return val;
}
```

* ir + cfg:

```
i32 foo (ptr str) {
entry:
  ptr %str.addr = salloc ptr
  write ptr str, ptr %str.addr
  return i32 0
}
i32 main (i32 argc, ptr argv) {
entry:
  ptr %val = salloc i32
  ptr %argv.addr = salloc ptr
  ptr %argc.addr = salloc i32
  write i32 argc, ptr %argc.addr
  write ptr argv, ptr %argv.addr
  write i32 0, ptr %val
  branch block loopcond
loopcond:
  i32 %tmp = read i32, ptr %val
  i1 %tmp1 = icmp neq, i32 %tmp, i32 0
  branch block loopbody, i1 %tmp1, block loopend
loopbody:
  write i32 33, ptr %val
  i32 %tmp2 = read i32, ptr %val
  i1 %tmp3 = icmp neq, i32 %tmp2, i32 0
  branch block ifthen, i1 %tmp3, block ifelse
ifthen:
  write i32 1, ptr %val
  branch block ifdone
ifelse:
  write i32 400, ptr %val
  branch block ifdone
ifdone:
  branch block loopcond
loopend:
  i32 %tmp4 = read i32, ptr %argc.addr
  i1 %tmp5 = icmp neq, i32 %tmp4, i32 0
  branch block ifthen1, i1 %tmp5, block ifdone1
ifthen1:
  ptr %tmp6 = read ptr, ptr %argv.addr
  ptr %tmp7 = read ptr, ptr %tmp6
  i32 %tmp8 = call i32 foo, ptr %tmp7
  write i32 %tmp8, ptr %val
  branch block ifdone1
ifdone1:
  i32 %tmp9 = read i32, ptr %val
  return i32 %tmp9
}
```

![cfg](https://github.com/arnaavgoyal/new-compiler/assets/58274830/dcef5f09-3e23-43a3-85d0-03baa0e6bc6d)

* dj-graph for `val`:

![djg](https://github.com/arnaavgoyal/new-compiler/assets/58274830/9c122857-865f-4e6d-99f9-fca7143a1997)

* final ir after stackpromote:

```
i32 foo (ptr str) {
entry:
  return i32 0
}
i32 main (i32 argc, ptr argv) {
entry:
  branch block loopcond
loopcond:
  i32 %val1 = phi block entry, i32 0, block ifdone, i32 %val4
  i1 %tmp1 = icmp neq, i32 %val1, i32 0
  branch block loopbody, i1 %tmp1, block loopend
loopbody:
  i1 %tmp3 = icmp neq, i32 33, i32 0
  branch block ifthen, i1 %tmp3, block ifelse
ifthen:
  branch block ifdone
ifelse:
  branch block ifdone
ifdone:
  i32 %val4 = phi block ifthen, i32 1, block ifelse, i32 400
  branch block loopcond
loopend:
  i1 %tmp5 = icmp neq, i32 argc, i32 0
  branch block ifthen1, i1 %tmp5, block ifdone1
ifthen1:
  ptr %tmp7 = read ptr, ptr argv
  i32 %val2 = call i32 foo, ptr %tmp7
  branch block ifdone1
ifdone1:
  i32 %val3 = phi block ifthen1, i32 %val2, block loopend, i32 %val1
  return i32 %val3
}
```

As you can see, all of the reads and writes to `%val` were eliminated and replaced with SSA registers and phi-nodes.

I fully intend to add more passes in the future, but for now, we need to actually generate code.

## Codegen
Codegen - the backend of my compiler - is still very much a work-in-progress.
So far, it naively converts from IR into an intermediate target representation.
The goal is that through multiple steps, it will end up in pure target code, after which it can simply be printed as assembly.

The fundamental concept is that each target subclasses `TargetCodeGen` and overrides the hooks.
From there, the subclassed `TargetCodeGen` object can be passed to Codegen, which will iteratively lower the code to target code using the target hooks.
Right now, I am working on the implementation of Codegen itself, as well as `X86TargetCodeGen`, which implements x86-64 target hooks.

## Current Functionality
Here are a few demonstrations of the current capabilities...

### Demo 1
This code:

```go
type my_i32 = i32;

var my_i32 glob = 0;

def (i8) my_i32
foo(num) {
    return num;
}

def (*i8) i32
bar(str) {
    var my_i32 i = 14;
    var u8 u = -4;
    var i32 v = foo(i);
    return v;
}

def (my_i32, **i8) i32
my_main(argc, argv) {
    var i32 v = &3;
    v = bar(*argv);
    var my_i32 v;
    v = foo(6;
    return v;
}
```

is synthesized into this AST:

![image](https://github.com/arnaavgoyal/new-compiler/assets/58274830/60e13cd0-cfaf-4c33-b972-6e789ca4eb77)

with these diagnostics:

![image](https://github.com/arnaavgoyal/new-compiler/assets/58274830/af86bd83-d640-41f8-8194-40a697a07239)

### Demo 2
This code:

```go
type my_i32 = i32;

def (i32) i32
foo(num) {
    return num;
}

def (*i8) i32
bar(str) {
    var i32 i = 14;
    var i32 v = foo(i);
    return v;
}

def (i32, **i8) i32
my_main(argc, argv) {
    var i32 v = 3;
    v = bar(*argv);
    return v;
}
```

is synthesized into this AST:

![image](https://github.com/arnaavgoyal/new-compiler/assets/58274830/3c8ffb51-4d5c-4db4-9fc0-8a72eb078f7e)

then translated into this IR:

```
i32 foo (i32 num) {
entry:
  ptr %num.addr = salloc i32
  write i32 num, ptr %num.addr
  i32 %tmp = read i32, ptr %num.addr
  return i32 %tmp
}
i32 bar (ptr str) {
entry:
  ptr %v = salloc i32
  ptr %i = salloc i32
  ptr %str.addr = salloc ptr
  write ptr str, ptr %str.addr
  write i32 14, ptr %i
  i32 %tmp = read i32, ptr %i
  i32 %tmp1 = call i32 foo, i32 %tmp
  write i32 %tmp1, ptr %v
  i32 %tmp2 = read i32, ptr %v
  return i32 %tmp2
}
i32 my_main (i32 argc, ptr argv) {
entry:
  ptr %v = salloc i32
  ptr %argv.addr = salloc ptr
  ptr %argc.addr = salloc i32
  write i32 argc, ptr %argc.addr
  write ptr argv, ptr %argv.addr
  write i32 3, ptr %v
  ptr %tmp = read ptr, ptr %argv.addr
  ptr %tmp1 = read ptr, ptr %tmp
  i32 %tmp2 = call i32 bar, ptr %tmp1
  write i32 %tmp2, ptr %v
  i32 %tmp3 = read i32, ptr %v
  return i32 %tmp3
}
```

which after stackpromote, becomes this IR:

```
i32 foo (i32 num) {
entry:
  return i32 num
}
i32 bar (ptr str) {
entry:
  i32 %v1 = call i32 foo, i32 14
  return i32 %v1
}
i32 my_main (i32 argc, ptr argv) {
entry:
  ptr %tmp1 = read ptr, ptr argv
  i32 %v1 = call i32 bar, ptr %tmp1
  return i32 %v1
}
```

which is translated into this intermediate target representation:

```
foo:
  return %s:16
bar:
  %v:0 = call %l:foo, %i:14
  return %v:0
my_main:
  %v:0 = read %s:20
  %v:1 = call %l:bar, %v:0
  return %v:1
```

which is lowered to this x86-64 target representation:

```
foo:
  %p:rax = x86:mov %s:16
  x86:ret <imp %p:rax>
bar:
  %s:4 = x86:mov %i:14
  %p:rax = x86:call %l:foo, <imp %s:4>
  x86:ret <imp %p:rax>
my_main:
  %v:0 = x86:mov %s:20
  %s:8 = x86:mov %v:0
  %p:rax = x86:call %l:bar, <imp %s:8>
  x86:ret <imp %p:rax>
```

which is finally outputted as this x86-64 assembly:

output.s
```asm
	.intel_syntax noprefix
	.globl foo
	.globl bar
	.globl my_main
foo:
	push	rbp
	mov	rbp, rsp
	sub	rsp, 0
	mov	rax, [rbp + 16]
	mov	rsp, rbp
	pop	rbp
	ret
bar:
	push	rbp
	mov	rbp, rsp
	sub	rsp, 4
	mov	[rbp - 4], 14
	call	foo
	mov	rsp, rbp
	pop	rbp
	ret
my_main:
	push	rbp
	mov	rbp, rsp
	sub	rsp, 8
	mov	rax, [rbp + 20]
	mov	[rbp - 8], rax
	call	bar
	mov	rsp, rbp
	pop	rbp
	ret
```

which... doesn't actually assemble:

test.c
```c
#include <stdio.h>
extern int my_main(int, char**);
int main() {
    int res = my_main(0, NULL);
    printf("%d\n", res);
    return 0;
}
```

terminal
```bash
$ gcc test.c output.s -o test.exe
output.s: Assembler messages:
output.s:17: Error: ambiguous operand size for `mov'
```

Hooray!

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
