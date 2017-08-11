# Bzo-Compiler
Compiler for a custom programming language I'm developing called Bzo


This file is of course not final, and will be updated as the project progresses.


*About Bzo*

Bzo is a programming language that I'm developing. The language is designed to be an imperative/functional language built on top of a purely functional core; imperative constructs like mutability are syntactic sugar for functional constructs like monads.

Bzo's main priorities are parallelism, performance, portability, and correctness. The languages that have influenced the language the most have been Haskell, Lisp, Rust, and Jai.

This compiler is still under VERY heavy development. Not all planned features are currently implemented. Currently, Linux is the only platform that is supported, though it may be runnable on other platforms as well.


*Running the Compiler*

Currently, the compiler can be run by calling the bzo executable in the /dist/build/main directory.
```
bzo main.bz
```
attempts to compile the main.bz file. Currently, "compilation" is limited to just lexing, parsing, and importing libraries.

```
bzo
```
opens a REPL-like environment, though it is limited to just lexing and parsing checks. Enter $quit to exit.


Libraries are stored in a /bzo file. The compiler checks in your /opt, /opt/lib, /opt/lib64, /lib, /lib64, /usr/lib, usr/lib64 directories. You can also pass in a -env=... parameter to the compiler with another (relative) path.

/bzo consists mainly of two subdirectories; /cfg, and /libs. /cfg holds config files, and /libs holds library code. For the most up-to-date library code, go to (https://github.com/charlesrosenbauer/Bzo-Standard-Library).



*Short-Term Plans / Features*

* I need to finish writing some type checking code, etc.

* Automatic memory management: Rust is a great example of how memory can be managed efficiently via compile-time checks. However, with a small amount of extra work, it should be possible to have a language capable of very efficient memory management without the need for traditional garbage collection passes. The language will make extensive use of persistent data structures and lifetime checks to assist here.

* Implicit Parallelism: rather than force the programmer to manage parallelism entirely on their own, Bzo aims to handle this with a smarter compiler. Implicit parallelism is definitely possible in a purely functional environment, however issues arise with deciding the proper granularity of threads; too high and threads aren't distributed efficiently, too low and the program generates too many threads for it to handle efficiently. Bzo aims to solve this problem by recognizing situations where threads can be generated in smaller numbers, or when they can be grouped together; for example, map operations only need to produce as many threads as the hardware supports, rather than how many can theoretically be run. Most other highger-order functions with any level of parallelism can be interpreted as variations on map operations, and therefore, thread numbers should be able to managed well simply with a smart enough compiler.

* LLVM backend: because LLVM is good for producing fast code, and offers a lot of cross-platform support.

* Unum support: As Bzo is a language oriented toward parallelism, it is necessary to have number formats that are parallelism-friendly. Unfortunately, floating point arithmetic is non-associative, which puts extreme limitations on parallelism-oriented optimizations. John Gustafson (https://en.wikipedia.org/wiki/John_Gustafson_(scientist)) has suggested a number format called Unums, which do not have this downside, and also have many other advantages. Including them in a parallelism-oriented language therefore makes a lot of sense, despite the current lack of hardware support for them.


*Long-Term Plans*

* Self-hosting: I've made some design decisions in this compiler that could be improved with a second revision, and I think a more permissive license would be favorable. As a result, this compiler will likely be used just for bootstrapping purposes, and a self-hosted compiler will be written later.

* Intermediate Representations: a goal for the self-hosted compiler will be to enable other languages to compile to an intermediate representation that can be compiled using the Bzo compiler. This will allow other languages to use features of Bzo, such as implicit parallelism, without having to reimplement it.

* Embedded Bytecode: the intermediate representation mentioned above will have a bytecode format that will be embedded into executable files. So long as dependencies can be met, this will allow applications to be recompiled for new platforms without needing the original source code.

* Compiler API: Much like Jonathan Blow's Jai language, Bzo is planned to offer a compiler API that enables Bzo code to have fine-grained control over what the compiler is doing. This enables very powerful debugging tools to be implemented with little effort, and enables automated code review at compile time.



*Syntax*

```
(A. B)
Compound Tuple


(A, B)
Polymorphic Tuple


[]I32
[4]I32
Array Types


{
	expr
	another expr
}
Do Block


expr : Type
Type Filter


;(parameters) :: definition
Lambda expression


x g f q
Function call (equivalent to q = f(g(x)) in C-like languages)

(a. b) f q
Function call (equivalent to q = f(a, b) in C-like languages)

(a. b) + q
Function call (equivalent to q = a + b in C-like languages)


Type :: Def
Simple Type Definition


(Pars) Type :: Def
Parameterized Type Definition


fn :: In ;; Out
Function Type Definition


fn :: expr
fn :: {expr}
(in) fn  :: expr
fn (out) :: expr
(in) fn (out) :: expr
...
Some Function definitions


f..
Map f to array


x`f
Partial application of f; behaves like a new function with x applied to the last argument of f.

4 5`+
Passing 4 into an addition function partially applied with 5. Returns 9.

(a. b) 12`%+
Here 12 is applied to %+, a modular addition function. This expression is desugared to (a. b. 12) %+.

a 2`12`%+
A variation on the previous example using multiple partially applied parameters. This desugars to (a. 2. 12) %+

4`+
This code is not valid however. Bzo does not allow *true* partial application. The resulting function cannot be returned. All inputs must be provided inline. This restriction is due to function overloading / polymorphic tuples.


()
Nil Type / Empty Tuple


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


$fn
Builtin Function


$Ty
Builtin Type


_
Wildcard Value; discard value in output


6423
0x35F
0b110011
03523
Some Integer Literals

53.2523
Floating Point Literal

"This is a comment"

'This is a String'

```


*Hello World*

```
Main $Module
IO   $import


main :: () ;; ()
main :: {
	~io:IO
	('Hello World!'. ~io) print ~io }

```

The first two lines initialize the file as the "Main" module, and import the IO library.

Following this, we define the type of the main function, such that it takes no inputs and gives no outputs.
```
main :: () ;; ()
```

Then we define main. Inside the do block, the first line defines a mutable variable ~io, and sets the type to IO. IO is a special type, and can only have one instance per function. A variable of the IO type can only be defined once outside of parameters. This is because the IO type is used to keep track of IO side effects; each function that performs IO actions must take it as an input, and return it as an output. This allows the compiler to easily track dependencies between functions for managing side effects, thus making parallelism easier.
```
~io:IO
```

The next line calls the print function, passing in a string, and the ~io variable. The print function returns an IO type, and so we store this back in ~io. This is also the end of the do block, and so the main function terminates here.
```
('Hello World!'. ~io) print ~io }
```
