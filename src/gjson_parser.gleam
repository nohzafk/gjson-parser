import gleam/bool
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

type Parser(a) =
  fn(String) -> Result(#(a, String), String)

// Monadic Infrastructure

/// `pure` creates a parser that succeeds immediately without consuming any input.
///
/// It “lifts” a `Real World `value into the `Parser World`.
pub fn pure(value: a) -> Parser(a) {
  fn(input: String) { Ok(#(value, input)) }
}

/// `bind` lets you sequence parsers where the next parser depends on the result of the previous one.
pub fn bind(parser: Parser(a), f: fn(a) -> Parser(b)) -> Parser(b) {
  // implementation 1
  // fn(input: String) {
  //   case parser(input) {
  //     Error(e) -> Error(e)
  //     Ok(#(value, rest)) -> f(value)(rest)
  //   }
  // }

  // implementation 2
  fn(input: String) {
    use #(value, rest) <- result.try(input |> parser)
    rest |> f(value)
  }
}

/// functor via `map`
/// A functor lets you transform the result of a parser without changing how it processes the input.
///
/// In practice, this means applying a function to the parsed value.
/// Essentially `map` applies a function in `Real World` to a value in `Parser World`
pub fn map(parser: Parser(a), f: fn(a) -> b) -> Parser(b) {
  // implementation 1
  // fn(input: String) {
  //   case parser(input) {
  //     Error(e) -> Error(e)
  //     Ok(#(value, rest)) -> Ok(#(f(value), rest))
  //   }
  // }

  // implementation 2
  // fn(input: string) {
  //   use #(value, rest) <- result.try(parser(input))
  //   Ok(#(f(value), rest))
  // }

  // implementation 3
  // defines map in terms of bind and pure (which is the standard monadic definition)
  bind(parser, fn(value) { f(value) |> pure })
}

/// applicative via `ap`
///
/// An applicative lets you apply a parser that produces a function to a parser that produces a value.
/// Essentially `ap` is the Function Application in the `Parser World`
///
/// <*> operator in Haskell
pub fn ap(func_parser: Parser(fn(a) -> b), value_parser: Parser(a)) -> Parser(b) {
  // implementation 1
  // fn(input: String) {
  //   use #(func, rest) <- result.try(func_parser(input))
  //   use #(value, rest) <- result.try(value_parser(rest))
  //   Ok(#(func(value), rest))
  // }

  // implementation 2
  // bind(func_parser, fn(func) {
  //   bind(value_parser, fn(value) { func(value) |> pure })
  // })

  // implementation 3
  // defines ap in terms of bind and pure (which is the standard monadic definition)
  use func <- bind(func_parser)
  use value <- bind(value_parser)
  func(value) |> pure
}

/// a map functor of two arguments
pub fn map2(
  parser1: Parser(a),
  parser2: Parser(b),
  f: fn(a, b) -> c,
) -> Parser(c) {
  // bind(parser1, fn(x) { bind(parser2, fn(y) { f(x, y) |> pure }) })

  use x <- bind(parser1)
  use y <- bind(parser2)
  f(x, y) |> pure
}

/// <* operator in Haskell
pub fn then_keep_left(parser1: Parser(a), parser2: Parser(b)) -> Parser(a) {
  bind(parser1, fn(value) { bind(parser2, fn(_) { value |> pure }) })
}

/// *> operator in Haskell
pub fn then_keep_right(parser1: Parser(a), parser2: Parser(b)) -> Parser(b) {
  bind(parser1, fn(_) { bind(parser2, fn(value) { value |> pure }) })
}

// Parser Primitives

/// A char parser matches a specific character.
pub fn char_parser(char: String) -> Parser(String) {
  fn(input: String) {
    case string.length(char) == 1 {
      False -> Error("char_parser expects a single character, got: " <> char)
      True -> {
        use c <- result.try(
          string.first(input)
          |> result.map_error(fn(_a) {
            "end of input, try to parser|" <> char <> "|"
          }),
        )
        case char == c {
          True -> Ok(#(c, input |> string.drop_start(1)))
          False -> Error("expected " <> char <> ", got " <> c)
        }
      }
    }
  }
}

/// A satisfy parser matches any character meeting a condition.
pub fn satisfy_parser(predicate: fn(String) -> Bool) {
  fn(input: String) {
    use char <- result.try(
      input |> string.first |> result.map_error(fn(_a) { "end of input" }),
    )
    case predicate(char) {
      True -> Ok(#(char, input |> string.drop_start(1)))
      False -> Error("DEBUG: |" <> char <> "|" <> "doesn't match predicate")
    }
  }
}

// core parser combinators

/// A or parser try one parser, if it fails, try another.
/// <|> opeator in Haskell
pub fn or(x: Parser(a), y: Parser(a)) -> Parser(a) {
  fn(input: String) { result.or(input |> x, input |> y) }
}

/// A many parser parses zero or more occurrences of a parser (returns a list).
pub fn many(parser: Parser(a)) -> Parser(List(a)) {
  // implementation 1
  // fn(input: String) {
  //   case input |> parser {
  //     // if the first parser fails, return an empty list and the original input
  //     Error(_) -> Ok(#([], input))
  //     Ok(#(value, rest)) -> {
  //       // recursive
  //       let next_parser = many(parser)
  //       case rest |> next_parser {
  //         // if the next parser fails, return the current value
  //         Error(_) -> Ok(#([value], rest))
  //         // combinate all values in a list
  //         Ok(#(values, rest2)) -> Ok(#([value, ..values], rest2))
  //       }
  //     }
  //   }
  // }

  // implementation 2
  // tail-recursive version with an accumulator
  fn(input: String) {
    input
    |> many_helper(parser, [])
    |> result.map(fn(pair) {
      let #(values, rest) = pair
      #(list.reverse(values), rest)
    })
  }
}

fn many_helper(parser: Parser(a), acc: List(a)) -> Parser(List(a)) {
  fn(input: String) {
    case input |> parser {
      Error(_) -> #(acc, input) |> Ok
      Ok(#(value, rest)) -> rest |> many_helper(parser, [value, ..acc])
    }
  }
}

/// A some parser one or more occurrences.
pub fn some(parser: Parser(a)) -> Parser(List(a)) {
  map2(parser, many(parser), fn(x, xs) { [x, ..xs] })
}

// could not use this mutul recursive definition, Gleam is not a lazy language
// fn many(parser: Parser(a)) -> Parser(List(a)) {
//   or(some(parser), pure([]))
// }

/// sequences turn a list of parser into a parser of list
pub fn sequences(parsers: List(Parser(a))) -> Parser(List(a)) {
  sequences_helper(parsers, [])
  |> map(fn(values) { values |> list.reverse })
}

fn sequences_helper(parsers: List(Parser(a)), acc: List(a)) -> Parser(List(a)) {
  case parsers {
    [] -> pure(acc)
    [head, ..rest] -> {
      bind(head, fn(value) { sequences_helper(rest, [value, ..acc]) })
    }
  }
}

fn sep_by1(parser: Parser(a), separator: Parser(b)) -> Parser(List(a)) {
  map2(parser, bind(separator, fn(_) { parser }) |> many, fn(x, xs) {
    [x, ..xs]
  })
}

// sep_by p sep = sep_by1 p sep <|> pure []
pub fn sep_by(parser: Parser(a), separator: Parser(b)) -> Parser(List(a)) {
  sep_by1(parser, separator) |> or(pure([]))
}

fn optional(parser: Parser(a)) -> Parser(Option(a)) {
  fn(input: String) {
    case input |> parser {
      Ok(#(value, rest)) -> #(Some(value), rest) |> Ok
      Error(_) -> #(None, input) |> Ok
    }
  }
}

// primitive parsers

fn string_parser(literal: String) -> Parser(String) {
  literal
  |> string.split("")
  |> list.map(char_parser)
  |> sequences
  |> map(string.concat)
}

fn whitespace() -> Parser(List(String)) {
  fn(c) { c == " " || c == "\t" || c == "\n" || c == "\r" }
  |> satisfy_parser
  |> many
}

/// Parses a token surrounded by optional whitespace
fn lexeme(parser: Parser(a)) -> Parser(a) {
  // implementation 1
  // First consume any leading whitespace, then run the parser, then consume trailing whitespace
  // bind(whitespace(), fn(_) {
  //   bind(parser, fn(value) { bind(whitespace(), fn(_) { pure(value) }) })
  // })

  // implementation 2
  whitespace() |> then_keep_right(parser) |> then_keep_left(whitespace())
}

// JSON Data Type

pub type JsonValue {
  JsonNull
  JsonBool(Bool)
  JsonNumber(Float)
  JsonString(String)
  JsonArray(List(JsonValue))
  JsonObject(List(#(JsonValue, JsonValue)))
}

// JSON-Specific Parsers

pub fn null_parser() -> Parser(JsonValue) {
  string_parser("null") |> lexeme |> map(fn(_) { JsonNull })
}

pub fn bool_parser() -> Parser(JsonValue) {
  or(string_parser("true"), string_parser("false"))
  |> lexeme
  |> map(fn(literal) {
    case literal {
      "true" -> JsonBool(True)
      _ -> JsonBool(False)
    }
  })
}

/// Combines two string parsers by concatenating their results
fn and_string(curr: Parser(String), next: Parser(String)) -> Parser(String) {
  map2(curr, next, fn(x, y) { x <> y })
}

pub fn number_parser() -> Parser(JsonValue) {
  let digit = fn(c) { "0123456789" |> string.contains(c) } |> satisfy_parser

  let sign_part =
    or(char_parser("+"), char_parser("-"))
    |> optional
    |> map(option.unwrap(_, ""))

  let integer_part = digit |> some |> map(string.concat)

  let decimal_part =
    char_parser(".")
    |> and_string(digit |> some |> map(string.concat))
    |> optional
    |> map(option.unwrap(_, ""))

  let exponent_part =
    or(char_parser("e"), char_parser("E"))
    |> and_string(sign_part)
    |> and_string(integer_part)
    |> optional
    |> map(option.unwrap(_, ""))

  fn(input: String) {
    use #(integer_part_str, rest) <- result.try(
      input |> { sign_part |> and_string(integer_part) },
    )

    use <- bool.guard(integer_part_str == "", return: Error(""))

    use #(decimal_part_str, rest) <- result.try(rest |> decimal_part)

    use #(exponent_part_str, rest) <- result.try(rest |> exponent_part)

    let value = integer_part_str <> decimal_part_str
    let value = case value |> string.contains(".") {
      False -> value <> ".0"
      True -> value
    }
    let value = value <> exponent_part_str
    case float.parse(value) {
      Ok(num) -> #(JsonNumber(num), rest) |> Ok
      _ ->
        Error("Failed to parse number|" <> value <> "|, rest: " <> rest)
        |> io.debug
    }
  }
  |> lexeme
}

fn is_hex_digit(char: String) -> Bool {
  case char |> string.to_utf_codepoints {
    [codepoint] -> {
      let code = string.utf_codepoint_to_int(codepoint)

      // Check if it's a digit (0-9): 48-57 in Unicode
      let is_digit = code >= 48 && code <= 57

      // Check if it's a lowercase hex letter (a-f): 97-102 in Unicode
      let is_lowercase_hex = code >= 97 && code <= 102

      // Check if it's an uppercase hex letter (A-F): 65-70 in Unicode
      let is_uppercase_hex = code >= 65 && code <= 70

      is_digit || is_lowercase_hex || is_uppercase_hex
    }
    _ -> False
  }
}

/// Parser for escape sequences
pub fn escape_sequence_parser() -> Parser(String) {
  let common_sequence =
    char_parser("\\")
    |> then_keep_right(
      satisfy_parser(fn(c) {
        c == "\"" || c == "\\" || c == "/" || "bfnrt" |> string.contains(c)
      }),
    )
    |> map(fn(c) {
      case c {
        "\"" -> "\""
        "\\" -> "\\"
        "/" -> "/"
        "b" -> {
          let assert Ok(b) = string.utf_codepoint(8)
          string.from_utf_codepoints([b])
        }
        "f" -> "\f"
        "n" -> "\n"
        "r" -> "\r"
        "t" -> "\t"
        _ -> panic as { "Unexpected char: " <> c }
      }
    })

  let unicode_escape_sequence =
    string_parser("\\u")
    |> then_keep_right(satisfy_parser(is_hex_digit) |> some)
    |> map(string.concat)
    |> fn(parser) {
      fn(input: String) {
        use #(hex_digits, rest) <- result.try(input |> parser)

        use number <- result.try(
          int.base_parse(hex_digits, 16)
          |> result.map_error(fn(_) {
            "Failed to parse hex digits|" <> hex_digits <> "|"
          }),
        )
        // Convert the code point to a Unicode character
        use codepoint <- result.try(
          string.utf_codepoint(number)
          |> result.map_error(fn(_) { "Failed to parse codepoint" }),
        )

        let parsed_char = string.from_utf_codepoints([codepoint])

        #(parsed_char, rest) |> Ok
      }
    }

  or(common_sequence, unicode_escape_sequence)
}

pub fn json_string_parser() -> Parser(JsonValue) {
  let no_special_char = satisfy_parser(fn(c) { c != "\"" && c != "\\" })
  let escape_char = escape_sequence_parser()
  let string_char = or(no_special_char, escape_char)

  char_parser("\"")
  |> then_keep_right(string_char |> many)
  |> then_keep_left(char_parser("\""))
  |> lexeme
  |> map(fn(values) { values |> string.concat |> JsonString })
}

// The issue is because Gleam is not a lazy language, there's a circular dependency between `array_parser()` and `json_parser()`.
// Since Gleam evaluates expressions eagerly, this creates an infinite recursion when they call each other.
//
// fn array_parser() -> Parser(JsonValue) {
//   char_parser("[")
//   |> then_keep_right(json_parser() |> sep_by(char_parser(",")))
//   |> then_keep_left(char_parser("]"))
//   |> lexeme
//   |> map(fn(values) { JsonArray(values) })
// }

// // Main entry point
// fn json_parser() -> Parser(JsonValue) {
//   choice([
//     null_parser(),
//     bool_parser(),
//     number_parser(),
//     json_string_parser(),
//     array_parser(),
//     object_parser(),
//   ])
// }

// The key is to actually consume the function return value
pub fn array_parser() -> Parser(JsonValue) {
  fn(input: String) {
    // First parse the opening bracket and whitespace
    use #(_, rest) <- result.try(input |> { char_parser("[") |> lexeme })

    // Parse JSON values separated by commas
    use #(values, rest) <- result.try(
      rest |> { json_parser() |> sep_by(char_parser(",")) },
    )

    // Parse closing bracket and whitespace
    use #(_, rest) <- result.try(rest |> { char_parser("]") |> lexeme })

    Ok(#(JsonArray(values), rest))
  }
}

fn object_item_parser() -> Parser(#(JsonValue, JsonValue)) {
  fn(input: String) {
    use #(key, rest) <- result.try(
      input
      |> { json_string_parser() |> then_keep_left(char_parser(":") |> lexeme) },
    )
    use #(value, rest) <- result.try(rest |> json_parser())

    #(#(key, value), rest) |> Ok
  }
}

pub fn object_parser() -> Parser(JsonValue) {
  fn(input: String) {
    // First parse the opening curly bracket and whitespace
    use #(_, rest) <- result.try(input |> { char_parser("{") |> lexeme })

    // Parse key-value piar
    use #(items, rest) <- result.try(
      rest |> { object_item_parser() |> sep_by(char_parser(",")) |> optional },
    )

    // Parse closing curly bracket and whitespace
    use #(_, rest) <- result.try(rest |> { char_parser("}") |> lexeme })

    #(JsonObject(items |> option.unwrap([])), rest) |> Ok
  }
}

// Main entry point
pub fn json_parser() -> Parser(JsonValue) {
  // Try each parser in sequence
  let assert Ok(parser) =
    [
      null_parser(),
      bool_parser(),
      number_parser(),
      json_string_parser(),
      array_parser(),
      object_parser(),
    ]
    |> list.reduce(or)

  parser |> lexeme
}

@external(erlang, "file", "read_file")
fn read_file(file_path: String) -> Result(String, Nil)

pub fn parse_json_file(path: String) {
  use content <- result.try(
    read_file(path)
    |> result.map_error(fn(_) { "Failed to read file: " <> path }),
  )
  content |> json_parser()
}

pub fn main() {
  todo
}
