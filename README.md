# Bytecode VM for a Dynamically Typed Language in Rust

## Overview

This project is an implementation of a bytecode virtual machine (VM) in Rust, designed for a dynamically typed language inspired by the [**Crafting Interpreters**](https://craftinginterpreters.com/) book by Robert Nystrom. This project covers chapter 2 of the book, implementing a bytecode VM for the langauge, the book was written using C and it was both englightening and challenging to convert the ideas and code into Rust. The project covers the entire process of parsing, compiling, and executing a simple scripting language on a bytecode VM. The book was a great educational resource and provides a good comparison between an AST based compiler and this chapter's single pass bytecode compiler.

This project furthered my experience in rust, providing further strength to my knowledge of lifetimes, function pointers, and further improving my understanding of Rust borrow checking rules as I worked the book's C code into a clean Rust implementation.

**Note:** This project is still a work in progress as I continue to work through the Crafting Interpreters book. Upon completion, I plan to refactor the codebase to enhance readability and optimize performance, addressing some initial, more experimental implementations.

## Educational Outcomes

This project served as an educational journey to deepen understanding of various computer science and Rust-specific concepts, including:

- **Pratt Parsing** - Implemented a Pratt parser to efficiently handle expressions with different precedences, learning how to create a flexible and easily extensible parser. Clean control flow was achieved via function pointers.
- **Compiler Rescue via Panic Mode** - Integrated error recovery techniques, allowing the compiler to recover from errors gracefully by moving to a safe point to continue parsing, thus allowing the compiler to produce a maximal number of compiler errors to assist the user.
- **Bytecode Compilation vs. AST Walking** - Gained a deeper understanding of VM architecture by comparing the AST walking approach implemented in Java and this project's single pass bytecode VM.
- **Rust Lifetimes and Memory Management** - Gained practical experience with Rust's lifetime annotations, ensuring safe and efficient memory management within the VM and compiler.
- **Macros** - Utilized Rust's macros as variadic helpers for emitting bytecode and performing common operations withinthe VM to reduce boilerplate and enhance code readability.
- **Dynamic Typeing** - Used Rust's union type to provide dynamic typing for the language whilst restricted to the static Rust types.
- **Functions and Control flow** - Learnt how functions and control flow for the language is implemented using jump instructions.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Acknowledgements

Special thanks to Robert Nystrom for his incredible work on **Crafting Interpreters**, which served as the foundation and inspiration for this project.
