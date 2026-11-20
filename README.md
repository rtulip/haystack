# Haystack

Haystack is a statically typed, stack-based programming language that adds opt-in variable assignment to improve ergonomics.
Haysatck was heavily inspired by [Porth](https://www.gitlab.com/tsoding/porth).
*Note*: This langue is is very early development and is subject to frequent and rapid changes.

# Getting Started:
Getting sarted with `Haystack` is pretty simple, there's two requirements: `rust` and `gcc`. I'm working in WSL, and can give no guarantee that `Haystack` will work properly on anything else. If you do give `Haystack` a try and something goes wrong, please open a bug and provide as many details as possible, and I'll do my best to try and figure it out. 

* `rust`: I'm using stable rustc version `1.100.0`, older versions should work, but I haven't tried to figure out what else works. See more about installing rust [here](https://www.rust-lang.org/tools/install). 
* `gcc`: I'm using version `8.3.0`. Again, I've not tested any older versions

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
Check out the [Tutorial](https://www.github.com/rtulip/haystack/wiki/Tutorial) to learn about how to use the language.

# Example Programs
Here's a few simple programs to show off a little of what `Haystack` looks like:

## Hello World
```rust
fn main() {
    "Hello World" println
}
```

## Generics 
```rust
fn dup<T>(T: t) -> [T T] { t t } 
fn swap<A B>(A: a B: b) -> [B A] { b a }
fn drop<T>(T: _) {}
```

## Pattern Matching
```rust
fn process<T>(T: _)    -> [Str] "Processing..." 
fn fallback()          -> [Str] "Fallback"
fn handle_err<E>(E: _) -> [Str] "An Error occurred" 

fn do_some_work<T E>(Result<Option<T> E>) -> [Str] match {
    Result::Ok match {
        Option::Some process
        Option::None fallback
    }
    Result::Err handle_err
}
```

## Easy C Interoperability
```rust
extern fn puts(&u8) -> [u64]

fn main() {
    "Hello from Haystack" Str.cstr puts drop
}
```

## Types as Values
```rust
fn hint<T>(T Type<T>) -> [T] drop

fn main() {
    Option.None #Option<u64> hint println
}
```