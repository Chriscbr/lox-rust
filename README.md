An implementation of Lox from Robert Nystrom's book "Crafting Interpreters", following the interpreter based approach, written in Rust.

This is a redone version from scratch after I previously tried implementing it in Rust [here](https://github.com/Chriscbr/lox-rust-old) and did not fully implement classes.
Unlike the first implementation, I made the parser and scanner more closely follow after the code from the book instead of trying to be more clever with adapting the code to Rust.

The main difference between this implementation and the version from the version in the book (besides language choice) is instead of implementing a resolver compiler pass, I opted to implement persistent environment data structures as suggested as an alternative in Chapter 11 ([link](https://craftinginterpreters.com/resolving-and-binding.html#persistent-environments)). In Rust this requires heavy use of `Rc<RefCell<T>>`.

This does result in some minor differences, as errors around variable initialization and using `super` or `return` in invalid contexts get surfaced at runtime instead of at compile time, but this seems OK to me.

I didn't get a chance to test the implementation against the entire Lox test suite (https://github.com/munificent/craftinginterpreters/tree/master/test) but I did add a decent number of my own tests to ensure a lot of the tricky edge cases around recursion and closures are handled.
