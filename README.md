# Haystack

Haystack is a statically typed, stack-based programming language that aims to improve upon my eariler experimental work of [tlp](https://www.github.com/rtulip/tlp).

*Note*: This langue is is very early development and is subject to frequent and rapid changes.

# Examples

## N'th Fibonacci Number
```
// No stack manipulation operations are built into the language
// apart from operators. So, for the moment, these need to be
// constructed manually. 
fn dup<T>(T: t) -> [T T] {t t}
fn drop<T>(T: t){} 

// Binding a variable consumes it from the stack, and makes it
// available until the end of the scope
fn fib(u64: x) -> [u64] {
    x 2 > if {
        x 1 - fib
        x 2 - fib
        +
    } else {
        x 
    }
}

fn main() {
    0 while dup 10 < {
        // You can also bind variables to a name outside of a
        // function signature. You can bind multiple items at
        // a time with `var [a b c ...]`
        var [n]
        n fib print
        n 1 +
    } 
}
```



