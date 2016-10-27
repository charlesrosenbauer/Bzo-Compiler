# Bzo-Compiler
Compiler for a custom programming language I'm developing called Bzo


The basic idea behind the language is that it is a functional / imperative hybrid; it allows you to write both purely functional and imperative code, all on top of a [mostly] functional core. Despite how difficult this sounds, the language is designed to be as simple as possible. The syntax is somewhere between Haskell and Lisp (mostly leaning toward Haskell), but with only 13 special characters, and no keywords besides some built-in functions and types prefixed with "$".

My long-term goal is have the language focus heavily on parallelism, and compile to multiple platforms. x86, ARM, RISC-V, and Adapteva's Epiphany are the main goals. That is of course supposing I get that far. I'm writing this all in Haskell, a language I'm still learning (despite the language's syntax resembling it). This compiler is a learning exercise for now.

Below is a basic rundown of some of what I have planned for the syntax. It is subject to change of course, and very little of it is currently implemented in the "compiler."


Some Ideas Behind the Language [WIP]

* Bzo manages side effects and mutable variables in the same way, by running functions referring to them more or less in the order written (it's actually a bit more complex than that), and running the rest of the code in parallel if possible.

* Particular side effects are managed by passing mutable "key" variables around. They may store information, but yet due to their status as mutable, the compiler enforces a strict order on their execution, preventing race conditions.

* The language, due to its focus on parallelism, uses arrays rather than lists.

* Functions work as monadic expressions. I.E : "f(g(x))" in a language like C is written as "x g f" in Bzo.

* Unlike Haskell, there is no currying. Tuples are used for passing variables in and out of functions.

* Functions can be transformed into array functions via adding ".." to the end.

* Tuples that contain only values of the same type can be interpreted as arrays.

* Polymorphic types, Algebraic types, and Containers are supported, but not Type Classes.

* Lambda expressions are of course supported. They are denoted with a single semicolon.

* Other features: pattern matching, references with Rust-like ownership and borrowing, and the ability to use what would normally be considered special characters in identifiers (so long as they aren't used in the normal syntax).


What Hello World will likely look like:

> "IO" $import
>
> main :: IO
>
> main :: c:~Console. ("Hello World!". c) println c

Explanation:
First we import the IO library.
Then we define main as type IO.
Then we define main's behavior all on one line ({} braces are required for multi-line functions). First, define a variable c, and set its type as mutable Console. Console is the "key" type for text IO. The Console type would also be fixed to only allow one variable of its type to be created to avoid issues. We pass the string "Hello World!" and c both into the println function, and pass the result (the "transformed" Console state) back into c.


Hypotenuse Function:

> hypot :: (Num.Num) ;; Num
> 
> (a.b) hypot q :: (a.b)^2..+ \2 q
>
> hypot :: ^2..+ \2

Explanation:
Okay, this is a bit complicated. In fact, I've written two implementations of it here. They do the same thing, but the bottom one just omits stuff the compiler could figure out anyway. First, we define the hypot function to take in inputs of type (Num.Num), and produce outputs of type Num.

Then we define hypot's behavior. The (a.b) before hypot are the input parameters, and the q afterward is the output parameter. It is then defined by taking the array, and passing it into the "^2" function, which squares numbers. By adding the ".." afterward, it is transformed into an array function, and because a and b are both presumed to be of the same type, the tuple can be interpreted as an array. Thus "^2.." returns a new tuple containing the squares of a and b respectively. That gets passed into the + function, which simply returns the sum of all its inputs. That gets passed into the "\2" function, which returns the square root of its input. This gets passed into q, which is of course the output parameter.

The second implementation, again, behaves the same. It just leaves the compiler to determine the inputs and outputs. In fact, without the specific type definition there (which wouldn't be manditory anyway), the second implementation could actually be more general than the first, as the compiler could find implementations for it on arrays (and tuples) of any size.




More info will be added soon.
