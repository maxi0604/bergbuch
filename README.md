# bergbuch: Robert Nystrom - Crafting Interpreters in Rust
[Crafting Interpreters](https://craftinginterpreters.com/) (free web version) by Robert Nystrom is a great book about compiler construction. In this repository I'm working through the book, but instead of writing Part 1 in Java and Part 2 in C, I'll try to write both parts in Rust. I think this has some advantages:

- It prevents mindless copy-pasting and deepens understanding
- It natively chooses the "functional style" approach to the expression problem, as opposed to the Java implementation using the Visitor pattern
- Rust
# Part 1
Part 1 is just the language specification, nothing to program there.

# Part 2
Part 2 (the tree walk interpreter) is somewhat complete. You can run it or its test suite as follows.

```sh
cargo run [file.lox]
cargo test
```
On the same machine, it's about three times slower than the author's `jlox` implementation (after replacing the default `HashMap`  with `FxHashMap`). I think this is fair, given that it has a simple object model with reference counting instead of decades of JVM optimizations under the hood.

# License
My code is licensed under MIT. If any significant (i. e. large enough to not be covered as a quote) parts of the book end up here, I do not intend to unlawfully relicense them.

# Contributions
Please work through the material yourself. Fixing small or stylistic errors would be welcome though.
