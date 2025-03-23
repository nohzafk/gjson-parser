# Gleam JSON Parser

This is a personal project implementing a JSON parser in Gleam using parser combinators and functional programming concepts.

It's not intended as a production-ready library, but rather as a learning exercise in:

- Parser combinator techniques
- Functional programming patterns (monads, functors, applicatives)
- Gleam language features

## Implementation Overview

The parser is built using a monadic parser combinator approach, with core components including:

- **Parser Primitives**: Basic parsers for characters, strings, and numbers
- **Combinators**: Higher-order parsers like `many`, `some`, `or`, and `sep_by`
- **JSON Parsers**: Specific parsers for JSON values (null, bool, number, string, array, object)
- **Monadic Infrastructure**: Implementations of `pure`, `bind`, `map`, and `ap`

The implementation demonstrates how to:
- Build complex parsers from simple ones
- Handle recursive JSON structures
- Manage parser state and error handling
- Apply functional programming patterns in a practical context

## Usage

To parse a JSON file:

```gleam
import gjson_parser

pub fn main() {
  gjson_parser.parse_json_file("example.json")
}
```

## Running Tests

The project includes test cases in `test/gjson_parser_test.gleam`. To run them:

```bash
gleam test
```

passed [json test suite 1](https://github.com/briandfoy/json-acceptance-tests/blob/master/json-checker/pass1.json)

## Dependencies

This project uses the standard Gleam library. No additional dependencies are required.


## Motivation 

Shout out to Tsoding's video which makes me want to learn parser combinators:

[JSON Parser 100% From Scratch in Haskell (only 111 lines)](https://youtu.be/N9RUqGYuGfw?si=3UHaaPHteQY0sqXc)
