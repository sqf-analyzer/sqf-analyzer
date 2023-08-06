# Design

## Passes

This parser is designed using the following passes:

### 1. Parse preprocessor

Implemented in Rust's `pest` crate to identify preprocessor tokens (e.g. `if`, `#if`, `+`).
Returns an iterator of `pest::Pair` each being a matched rule (that may include multiple tokens).

Implemented in [./preprocessor/parser.rs](./preprocessor/parser.rs).

### 2. Build preprocessor AST

Implemented in custom code, receives an iterator of `pest::Pair` and
returns a preprocessor abstract syntatic tree [`Ast`](./preprocessor/ast.rs)
describing the different preprocessor operators. The leafs of the tree are `Spanned<&str>`, where
`Spanned` contains the position of each of the leaf in the file.

Implemented in [./preprocessor/parser.rs](./preprocessor/parser.rs).

### 3. Process preprocessor AST to SQF tokens

Implemented in custom code, receives an `Ast` and returns an iterator of `Spanned<Cow<str>>`.
The sequence of items of this iterator corresponds a sequence of SQF tokens, for example

`["private", "\"_a\"", "=", "1", ";"]`

This iterator fully evaluates the preprocessor rules during iteration, performing macro substitution
and replacement (evaluating `#include`, `#define`, `#ifdef`, macros, etc).

This iterator also exposes any errors found during the processing
(e.g. macro calls with missing arguments).

Implemented in [./preprocessor/iterator.rs](./preprocessor/iterator.rs).

### 4. Parse of SQF tokens to an SQF AST

Implemented as a Pratt parser, receives a sequence of SQF tokens
and returns an SQF abstract syntatic tree (denoted `Expr`),
resolving all parenthesis and balancing precedence.

Implemented in [./parser/pratt.rs](./parser/pratt.rs).

### 4. Analyze SQF AST

Receives an `Expr` and produces a `State` describing different of properties of the AST, such as

* Type inference of each variable
* Origin of each variable
* Errors

Implemented in [./analyzer.rs](./analyzer.rs).
