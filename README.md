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

# Example Programs
Here's a few simple programs to show off a little of what `Haystack` looks like:

## Hello World
```
include "std.hay"
fn main() {
    "Hello World" println
}
```

## N'th Fibonacci Number
```
fn fib(u64: n) -> [u64] {
    n 2 < if {
        n
    } else {
        n 1 - fib
        n 2 - fib +
    }
}
```

## Print Numbers 1 to 10
```
fn main() {
    1 while dup 10 <= {
        as [i]
        i println
        i 1 +
    } drop
}
```

# Tutorial:
Check out the [Tutorial](https://www.github.com/rtulip/haystack/wiki/Tutorial) to learn about how to use the language

