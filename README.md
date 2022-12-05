# Haystack

Haystack is a statically typed, stack-based programming language that adds opt-in variable assignment to improve ergonomics.
Haysatck was heavily inspired by [Porth](https://www.gitlab.com/tsoding/porth).
*Note*: This langue is is very early development and is subject to frequent and rapid changes.

# Getting Started:
Getting sarted with `Haystack` is pretty simple, there's three requirements: `rust`, `nasm`, and `ld`. I'm working on Ubuntu, and can give no guarantee that `Haystack` will work properly on anything else. If you do give `Haystack` a try and something goes wrong, please open a bug and provide as many details as possible, and I'll do my best to try and figure it out. If you're using Windows, I recommend using `virtualbox` or something similar and get a Linux virtual machine until Windows is a supported target.

* `rust`: I'm using nightly rustc version `1.66.0`, older versions should work, but I haven't tried to figure out what else works. See more about installing rust [here](https://www.rust-lang.org/tools/install). Once you've installed rust, use the command `rustup override set nightly` to switch to the nightly version of the rustc compiler.
* `nasm`: I'm using version `2.13.02`. I was able to install it using `apt`.
* `ld`: I believe this was installed with the rest of the GNU Binutils, on `Ubuntu`. 

## Install the project

You can clone the project from github
* `git clone https://www.github.com/rtulip/haystack`

## Run the tests
`cd` into the haystack directory, and run the tests with `cargo t`.
The tests run each of the `.hay` files under `src/tests` to make sure they produce the expected outputs. If all the tests pass you should be good to write your own programs using `Haystack`.

## Compile and Run examples

You can try any of the examples under `./examples`.
* You can compile the program with: `cargo r -- examples/name_of_file.hay`
* Add the `-r` or `--run` flag to also run the program:  ` cargo r -- examples/name_of_file.hay -r`

# Tutorial:

## Basics

`Haystack` is a stack based language. This means that operations either consume from or produce onto a data stack. As such, `Haystack` uses reverse polish notation for most operations. For example, here's a simple program which adds two numbers together and prints the result to stdout.

```
include "std.hay"
fn main() {
    1 2    // literals get pushed onto the stack.
    +      // `+` consumes two `u64` from the top of the stack, and pushes the resuling sum back onto the stack.
    putlnu // The `putlnu` function consumes a `u64` from the top of the stack and prints it to stdout.
}
```

`Haystack` is statically typed, so the type checker which will make sure that you've provided the correct types for any given operation or function call.
``` 
// doesn't compile
fn main() {
    1 true + 
}
```

Because manipulating items solely on the stack can become very tedious and can add a significant mental burden, you are able to create scoped variables using the `as [ident ..]` syntax. Variables will last for the duration of their scope, and are cosumed from the top of the stack. 

```
include "std.hay"
fn main() {
    1 2
    as [one two]
    one putlnu
    two putlnu
    two putlnu
    one putlnu
}
// outputs 1 2 2 1 to stdout
```

## Branching
The `if` keyword will consume a `bool` from the top of the stack and will fallthrough into the block if it's `true`. `Haystack` requies that each branch of the `if` block produces a similar stack.

The syntax is: `<cond> if { ... } else ... <cond> if { ... } else { ... }`

```
include "std.hay"
// This compiles fine
fn main() {

    1 2 as [a b]
    a b < if {
        a
    } else a b > if {
        b 
    } else {
        a b +
    }

    putlnu

}
```

```
// This doesn't compile
fn main() {
    true if {
        1
    } else {
        true
    }
}
```

## Loops

Only `while` loops are supported at this time.

```
include "std.hay"
// prints number up to 10
fn main() {
    0 while dup 10 < {
        as [i]
        i putlnu
        i 1 +
    } drop
}
```

`Haystack` requires that the stack starting from the `whlie` keyword is similar to the stack at the end of the block. 
```
// This doesn't compile
fn main() {

    while true {
        1
    }

}
```

## Functions
As seen in the earlier examples, f unctions are defined with the `fn` keyword followed by the definition:

`fn name<T1 T2...>(InputType1: ident1 InputType2: ident2 ...) -> [OutputType1 OutputType2 ...] { Function Body }`

The type annotations `<T1 T2 ...>` are optional and only needed for making generic functions. Input arugments can be optionally given an identifier, to which the argument will be automacially bound. However, if any arguments are given an identifier, all arguments must be given an identifier. If a produces any outputs, the output types are placed in the output list without nay annotations. If the no outputs are needed, the output list shouldn't be given. Below are some examples, showing how this syntax can be used.

```
fn add_wrapper(u64 u64) -> [u64] { + } // unidentified arguments remain on the stack.
fn add_wrapper2(u64: a u64: b) -> [u64] { a b + }
fn duplicate<T>(T: t) -> [T T] { t t }
fn shuffle<A B C>(A: a B: b C: c) -> [B C A] { b c a }
```

Here's how you call a function:
```
include "std.hay"
fn add_wrapper(u64 u64) -> [u64] { + }

fn main() {
    1 2 add_wrapper putlnu // prints 3
}
```

Again, the type checker is your friend, and will help make sure that each function produces the expected output, and has the expected inputs on the stack before the call is made.

## Structures

Sometimes, it's useful to group data together into logical components. For example, strings in `Haystack` are represented internally as the following structure:
```
struct Str {
    pub u64: size
    pub *u8: data
}
```
Structured items are considered as one element on the stack. Here's how you can create a structure in `Haystack` using the `cast({type})` operation.

```
// Note: Haystack requires that struct elements have an identifier.
struct Pair<T> {
    T: first
    T: second
}

fn main() {

    1 2 cast(Pair)              // creates a Pair<u64>
    "Hello" "World" cast(Pair)  // creates a Pair<Str>
    // ...
}
```

You can access public struct members directly from an `as` block:

```
include "std.hay"
struct Pair<T> {
    pub T: first
    pub T: second
}

fn main() {

    "Hello" "World" cast(Pair)
    as [pair]

    pair::first::size putlnu

    // the `putlns` function is defined in "std.hay"
    // and prints a `Str` to stdout with a newline.
    pair::second      putlns 
}
```

## Including other files

You can include files with the `include "path/to/file.hay"` syntax. `Haystack` will search from `src/libs/` as well as the current directory for the file.

```
// defines 'putlnu' function
include "std.hay" 

fn main() { "Hello World!" putlnu }
```

## The Prelude 

There are some functions which are automatically compiled with any `Haystack` program. You can find these under `src/libs/prelude.hay`. These functions are there to provide common stack operations, so that you don't need to redefine the most common functions, or always have them included. 

```
fn drop<T>(T: t) {}                         // removes the top element from the stack
fn dup<T>(T: t) -> [T T] { t t }            // duplicates the top element of the stack 
fn swap<A B>(A: a B: b) -> [B A] { b a }    // swaps the order of the top two elements of the stack.
fn ptr+<T>(*T: ptr u64: n) -> [*T] { ... }  // offsets a pointer by n
```

# Examples

## Hello World

```
include "std.hay"

fn main() {
    "Hello World!\n" putlnu
}
```

## Print all numbers up to N

```
include "std.hay"
fn main() {
    
    100 as [n]
    0 while dup n < {
        as [i]
        i putlnu
        i 1 +
    }
}
```

## N'th Fibonacci Number
```
fn fib(u64: x) -> [u64] {
    x 2 > if {
        x 1 - fib
        x 2 - fib
        +
    } else {
        x 
    }
}
```



