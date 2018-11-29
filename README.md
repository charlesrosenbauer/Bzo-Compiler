# Bzo-Compiler
Compiler for a custom programming language I'm developing called Bzo

This file is of course not final, and will be updated as the project progresses.


*About Bzo*

Bzo is a programming language that I'm developing. The language is designed to be an imperative/functional language built on top of a purely functional core; imperative constructs like mutability are syntactic sugar for functional constructs like monads.

Bzo's main priorities are parallelism, performance, portability, and correctness. The languages that have influenced the language the most have been Haskell, Lisp, Rust, and Jai.

This compiler is still under VERY heavy development. Not all planned features are currently implemented. Currently, Linux is the only platform that is supported, though it may be runnable on other platforms as well.


Also, if you have any knowledge of Haskell and are interested in contributing, take a look in info/contributing.txt. I'll try to keep that up to date.


*Running the Compiler*

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



*Short-Term Plans / Features*

* I need to finish writing some type checking code, etc.

* Simple Golang backend: the analysis that Bzo will depend on for its more powerful features doesn't seem easy to do in Haskell. Those features thus will be pushed onto the eventual self-hosted version. This version will simply output and compile Golang, which will allow Bzo to piggyback off of Go's GC so that memory analysis can be saved until later.


*Long-Term Plans*

* Self-hosting: I've made some design decisions in this compiler that could be improved with a second revision, and I think a more permissive license would be favorable. As a result, this compiler will likely be used just for bootstrapping purposes, and a self-hosted compiler will be written later.

* Implicit Memory Arena Usage: Rust is a great example of how memory can be managed efficiently via compile-time checks. However, languages like C are often still faster due to extensive usage of memory arenas and allocators. With a small amount of extra analysis, it should be possible to have a language capable of implicitly deciding when this is feasible through some dependency analysis, etc.

* Implicit Parallelism: rather than force the programmer to manage parallelism entirely on their own, Bzo aims to handle this with a smarter compiler. Implicit parallelism is definitely possible in a purely functional environment, however issues arise with deciding the proper granularity of threads; too high and threads aren't distributed efficiently, too low and the program generates too many threads for it to handle efficiently. Bzo aims to solve this problem by recognizing situations where threads can be generated in smaller numbers, or when they can be grouped together; for example, map operations only need to produce as many threads as the hardware supports, rather than how many can theoretically be run. Most other highger-order functions with any level of parallelism can be interpreted as variations on map operations, and therefore, thread numbers should be able to managed well simply with a smart enough compiler.

* Intermediate Representations: a goal for the self-hosted compiler will be to enable other languages to compile to an intermediate representation that can be compiled using the Bzo compiler. This will allow other languages to use features of Bzo, such as implicit parallelism, without having to reimplement it.

* Embedded Bytecode: the intermediate representation mentioned above will have a bytecode format that will be embedded into executable files. So long as dependencies can be met, this will allow applications to be recompiled for new platforms without needing the original source code.

* Compiler API: Much like Jonathan Blow's Jai language, Bzo is planned to offer a compiler API that enables Bzo code to have fine-grained control over what the compiler is doing. This enables very powerful debugging tools to be implemented with little effort, and enables automated code review at compile time.

* Direct Backend: LLVM may be powerful, but it's also slow and makes assumptions about the programming language that are much more pessimistic than what is achievable in a purely functional language. A backend that directly produces machine code should be faster, and allow for more freedom with optimization. Some potential optimization techniques that could be utilized include Cached Conditionally-Correct Superoptimization, and various forms of Function Fusion.



*Syntax*

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


x`f
Partial application of f; behaves like a new function with x applied to the last argument of f.
Due to ambiguities arising from the type system, all parameters but the first one must be passed in this way.

4 5`+
Passing 4 into an addition function partially applied with 5. Returns 9.

a b`12`%+
Here b and 12 is applied to %+, a modular addition function. This expression is desugared to (a. b. 12) %+.

[a, b] c`%+
This code is not a valid alternative however.

a b c`%+
Neither is this. Only one parameter must remain after partial application.


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
