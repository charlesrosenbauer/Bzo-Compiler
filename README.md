# Bzo-Compiler
Compiler for a custom programming language I'm developing called Bzo


Note: This branch is under heavy development and likely will be very unstable. The parser generators being used in the main branch do not provide the necessary amount of information for useful error messages, and are in need of some significant bug fixes anyway. This branch is an attempt to rewrite them entirely in normal Haskell, without the use of parser generators. This should provide more flexibility, more freedom, and easier maintenence.


Building and Trying Out the Compiler

If you're interested in trying out what's currently there (not much), you may build the program using the provided makefile. The output executable file may show up in the /src directory. The resulting program can be run in a command line. If parameters are provided, it will attempt to interpret each parameter as a file name, then print the results of the lexer to the command line. If none are provided, the program will simply enter the REPL, allowing Bzo expressions to be entered, providing the user with the results from the lexer. Enter $quit to exit.


The Language

The basic idea behind the language is that it is a functional / imperative hybrid; it allows you to write both purely functional and imperative code, all on top of a [mostly] functional core. Mutability is handled not unlike Uniqueness Types in languages like Clean, Mercury, and Idris. The language is designed to be as simple as possible. The syntax is somewhere between Haskell and Lisp (mostly leaning toward Haskell), but with only 13 special characters, and no keywords besides some built-in functions and types prefixed with "$".

My long-term goal is have the language focus heavily on parallelism, and compile to multiple platforms. x86, ARM, RISC-V, and Adapteva's Epiphany architecture are the main goals. That is of course supposing I get that far. I'm writing this all in Haskell, a language I'm still learning (despite the language's syntax resembling it). This compiler is a learning exercise for now.

Below is a basic explanation of some of what I have planned for the syntax. It is subject to change of course, and very little of it is currently implemented in the "compiler."


Some Ideas Behind the Language [WIP]

* Bzo manages side effects and mutable variables in the same way, by running functions referring to them more or less in the order written (it's actually a bit more complex than that), and running the rest of the code in parallel if possible.

* Mutable variables are (as mentioned above) handled similarly to Uniqueness types.

* References are handled somewhat similarly to Rust's pointer borrowing, though somewhat simplified. References can be treated like Uniqueness types, when passed as a normal value, but when passed into another reference (let's call it B, and the original A), A is set to null, and ownership of their data is passed to B. If B had ownership of any data, it is destroyed. Effects of mutability still apply however, so passing references may or may not apply. A references always point to mutable data, though the reference itself may or may not be mutable.

* Particular side effects are managed by passing mutable "key" variables around. They may or may not store information, but yet due to their status as mutable, the compiler enforces a strict order on their execution, preventing race conditions.

* The language, due to its focus on parallelism, uses arrays rather than lists.

* Functions work as monadic expressions (like APL). I.E : "f(g(x))" in a language like C is written as "x g f" in Bzo.

* Unlike Haskell, there is no currying. Tuples are used for passing variables in and out of functions.

* Functions can be transformed into array functions via adding ".." to the end.

* Tuples that contain only values of the same type can be interpreted as arrays.

* Polymorphic types, Algebraic types, and Containers are supported, but not Type Classes.

* Lambda expressions are of course supported. They are denoted with a single semicolon.

* Other features: pattern matching, references with Rust-inspired ownership and borrowing, and the ability to use what would normally be considered special characters in identifiers (so long as they aren't used in the normal syntax).


All symbols and their (current) intended meanings:
```
(  )
Tuples

[  ]
Array Access

{  }
Do Block

:
Type Filter (i.e., x:I32 forces x to be of type I32.)

;
Denote Lambda Expression

::
Define

;;
Denote Function Type (i.e., (I32.I32);;I32 is the type of a function that takes (I32.I32) as
an input, and produces I32 as an output.)

.
Expression Separator. This works a bit like a semicolon in other languages.

,
Option Separator. This is like an Expression Separator, but used specifically in Polymorphic
Types, similar to | in Haskell type definitions.

..
Convert to Array Function

"..."
Strings

'...'
Comment

()
Nil Type / Empty Tuple

[]
General Array modifier. Used to denote an array type without specifying size

@
Reference Modifier

~
Mutability Modifier

_
Wildcard. Used for pattern matching, as it is in many other languages.

$...
Denote Builtin; this character is only available for use in identifiers for types and functions
built in to the language. Most types and functions that use builtins will be given an alias.

```
What Hello World will likely look like:
```
"IO" $import

main :: IO

main :: c:~Console. ("Hello World!". c) println c
```
Explanation:
First we import the IO library.
Then we define main as type IO.
Then we define main's behavior all on one line ({} braces are required for multi-line functions). First, define a variable c, and set its type as mutable Console. Console is the "key" type for text IO. The Console type would also be fixed to only allow one variable of its type to be created to avoid issues. We pass the string "Hello World!" and c both into the println function, and pass the result (the "transformed" Console state) back into c.


Hypotenuse Function:
```
hypot :: (Num.Num) ;; Num

(a.b) hypot q :: (a.b)^2..+ \2 q

hypot :: ^2..+ \2
```

Explanation:
Okay, this is a bit complicated. In fact, I've written two implementations of it here. They do the same thing, but the bottom one just omits stuff the compiler could figure out anyway. First, we define the hypot function to take in inputs of type (Num.Num), and produce outputs of type Num.

Then we define hypot's behavior. The (a.b) before hypot are the input parameters, and the q afterward is the output parameter. It is then defined by taking the array, and passing it into the "^2" function, which squares numbers. By adding the ".." afterward, it is transformed into an array function, and because a and b are both presumed to be of the same type, the tuple can be interpreted as an array. Thus "^2.." returns a new tuple containing the squares of a and b respectively. That gets passed into the + function, which simply returns the sum of all its inputs. That gets passed into the "\2" function, which returns the square root of its input. This gets passed into q, which is of course the output parameter.

The second implementation, again, behaves the same. It just leaves the compiler to determine the inputs and outputs. In fact, without the specific type definition there (which wouldn't be manditory anyway), the second implementation could actually be more general than the first, as the compiler could find implementations for it on arrays (and tuples) of any size.




More info will be added soon.
