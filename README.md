# A Toy Language

This is a simple toy language based on Ocaml.

<hr>

### BASIC

This is a simple toy calculator with two modes of computation:

- **An interpreter**
- **A compiler and a stack machine**

### LEXER

This is the lexer or tokenizer for the language, which tokenizes a given expression into tokens which will be read by the Parser.

### PARSER

This tool parses the lexical tokens created by the lexer and creates an abstract syntax tree (ast). This specific parser is designed for an object language similar to the Prolog language.

### INTERPRETER

This is the interpreter for the simple Prolog-like language that uses the lexer and parser above.

<hr>
