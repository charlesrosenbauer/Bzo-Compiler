# Bzo-Compiler
---

Compiler for a custom programming language I'm developing called Bzo

This file is of course not final, and will be updated as the project progresses.


**About Bzo**

Bzo is a programming language that I'm developing with the goal of rethinking the role of the compiler.

Traditionally, the role of a compiler has been to serve as a black box that takes code in and returns a large list of error messages (or in rare cases, an executable file) out. However, as it is becoming increasingly important to make software that is correct, secure, and efficient, it is becoming necessary to leverage more powerful tools in the software development process.

Compilers often perform a lot of powerful analysis on code, but being a black box, that information is rarely available to the programmer (and even when it is, it's not easy to access!). Making this information available to the programmer, and perhaps also providing the programmer with tools to be able to directly interact with this analysis (e.g, code assertions that can statically verify that say, a variable x is not equal to 0), would likely serve to be a very powerful tool in improving security and correctness.

In the case of code optimization, this is mostly a problem of automation. The standard way to perform optimization is to generate a very inefficient IR from the original code, and then to progressively optimize it with analysis-gated transformations. Which transformations are applied is often not well-defined and subject to change, and how much control the programmer has over them is very limited. While this works fine for most applications, it takes a lot of control away from the programmer to properly engineer their software to be as efficient as possible.

It is very common for a programmer aiming for high performance to have to contort the language in ways it was never intended to be written in in order to achieve even a fraction of the potential performance, often making the code vastly more difficult to understand and maintain in the process. Many of these cases could be alleviated by simply providing the programmer with more powerful metaprogramming tools, or even the ability to write their own compiler passes.

My vision for Bzo is for the compiler to be more of a toolset for building more sophisticated build and compilation systems. A frontend will serve as a way of converting human-readable code to a computer-manipulable IR, and backends will exist for converting an IR to machine code for different architectures. An additional set of middle-end tools; optimization passes, analysis passes, profiling and instrumentation tools, formal verification, code visualizations, etc. will exist to allow the programmer to more thoroughly understand and control how code is generated. Custom middle-end tools, as well as even custom frontends and backends could be developed as well.


**Running the Compiler**

Currently, the compiler can be run by calling the bzo executable in the /dist/build/main directory.
```
./dist/build/main/main main.bzo
```
attempts to compile the main.bzo file. Currently, "compilation" is limited to just lexing, parsing, desugaring, and importing libraries.

```
./dist/build/main/main
```
opens a REPL-like environment, though it is limited to just lexing and parsing checks. Enter $quit to exit.


Libraries are stored in a bzo/ file. The compiler checks in your /opt, /opt/lib, /opt/lib64, /lib, /lib64, /usr/lib, usr/lib64 directories. You can also pass in a -env=... parameter to the compiler with another (relative) path.

bzo/ consists mainly of two subdirectories; cfg/, and libs/. cfg/ holds config files, and libs/ holds library code. For the most up-to-date library code, go to (https://github.com/charlesrosenbauer/Bzo-Standard-Library).

The compiler can be run in parallel by adding the parameters "+RTS -NX" where X is the number of CPU cores you wish you use. This does have some drawbacks however, mainly that it only currently handles parallelism across full files (meaning you won't see much of an improvement in compilation speed without many large program files), and that it is not extremely stable.



**Short-Term Plans / Features**

* Finish typechecking and IR generation in the frontend.

* Get the backend (https://github.com/charlesrosenbauer/Bzo-Backend) to output x86 machine code.

* Get a basic superoptimization engine (https://github.com/charlesrosenbauer/Superoptimizer) working for code optimization.


**Medium-Term to Long-Term Plans**

* Self-hosted compiler with a more permissive license and some minor syntax revisions.

* A powerful analysis engine with an assertion-based code checking system.

* A simple, extensible IDE with some basic code visualization tools.

* The compiler needs an API in the standard library to allow the programmer to properly control the generation of their code.


**Syntax**

```
[A, B]
Compound Tuple


(A, B)
Polymorphic Tuple


[:I32]
[4:I32]
Array Types


{
	expr
	another expr
}
Do Block


expr . Type
Type Filter. This is a way of adding constraints to a type, for example that it must be a member of a type class, or that it is one of several options in a polymorphic type. These filters are normally not allowed outside of function/type parameters.


;parameter{definition}
;[par0,par1]{definition}
Lambda expression


x g f q
Function call (equivalent to q = f(g(x)) in C-like languages)

[a, b] f q
Function call (equivalent to q = f(a, b) in C-like languages)

[a, b] + q
Function call (equivalent to q = a + b in C-like languages)

[+: a, b] q
Prefix function call (equivalent to q = a + b). This is just an alternative way to call functions that is more readable in some cases.

[a, c] [_, b, _, d]
[a, d] [+: _, b, c, _]
Wildcards in expressions can be used to more cleanly separate expressions without nesting tuples.


Type :: Def
Simple Type Definition


[Pars'] Type :: Def
Parameterized Type Definition


[A'] Functor :: {
	[F'.Functor] map :: [A' F', [A' ;; B']] ;; B' F'
}
Type Class Definition


fn :: In ;; Out
[K'.Ord, A'] insert :: [[K', A']Dict, K', A'] ;; [K', A']Dict
Function Type Definitions


fn :: expr
fn :: {expr}
[in] fn  :: expr
fn [out] :: expr
[in] fn [out] :: expr
...
Some Function definitions


f..
Map f to array

x [_, y] [q, r]
Hole expressions; values can be propagated through the expression using holes.

[a, c] [_, b, _, d] [w, x, y, z]
Holes can even be used for filling complex patterns.

[+: a, b]
For readability, expressions may also be formatted in a kind of prefix notation.

xs sort [take: 5, _] ys
Holes can be included in prefix expressions.


()
[]
Nil Type / Empty Tuple (Both are acceptable)


T@Namespace
Namespace Indicator


fn
Function/variable identifier (must start with non-special symbol, or lowercase letter)


Ty
Type Identifier (must start with capital letter)


T'
Type Variable (Type Identifier ending with single quote)


~var
Mutable variable (Function/variable identifier starting with tilde)


#fn
Builtin Function


#Ty
Builtin Type


_
Wildcard Value; discard value in output


6423
-312
0x35F
0b110011
03523
Some Integer Literals - Negatives are only allowed in decimal format

53.2523
-2.15
Some Floating Point Literals

"This is a comment"

'This is a String'

```


*Hello World*

```
Main #Module
IO   #import


main :: () ;; ()
main :: {
	() IO ~io
	['Hello World!', ~io] print ~io }

```

The first two lines initialize the file as the "Main" module, and import the IO library.

Following this, we define the type of the main function, such that it takes no inputs and gives no outputs.
```
main :: () ;; ()
```

Then we define main. Inside the do block, the first line calls the IO constructor (which takes no parameters, so we just pass in a Nil), and we store it in a mutable variable, ~io. IO is a special type, and can only have one instance per function. A variable of the IO type can only be defined once outside of parameters. This is because the IO type is used to keep track of IO side effects; each function that performs IO actions must take it as an input, and return it as an output. This allows the compiler to easily track dependencies between functions for managing side effects, thus making parallelism easier.
```
() IO ~io
```

The next line calls the print function, passing in a string, and the ~io variable. The print function returns an IO type, and so we store this back in ~io. This is also the end of the do block, and so the main function terminates here.
```
['Hello World!', ~io] print ~io }
```
