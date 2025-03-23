import gjson_parser.{
  JsonBool, JsonNull, JsonNumber, JsonString, ap, array_parser, bool_parser,
  char_parser, escape_sequence_parser, json_parser, json_string_parser, many,
  map, null_parser, number_parser, object_parser, or, parse_json_file, pure,
  satisfy_parser, sep_by, sequences, some, then_keep_left, then_keep_right,
}
import gleam/list
import gleam/string
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn map_test() {
  let add = fn(x: Int) { x + 1 }
  "" |> map(pure(3), add) |> should.equal(Ok(#(4, "")))
}

pub fn ap_test() {
  let uppercase = fn(a: String) { string.uppercase(a) } |> pure
  "a" |> ap(uppercase, char_parser("a")) |> should.equal(Ok(#("A", "")))
}

// Parser Primitive Tests
pub fn char_parser_test() {
  "h"
  |> char_parser("h")
  |> should.equal(Ok(#("h", "")))
}

pub fn satisfy_parser_test() {
  "0"
  |> satisfy_parser(fn(c) { c == "0" })
  |> should.equal(Ok(#("0", "")))
}

pub fn or_parser_test() {
  "e"
  |> or(char_parser("e"), char_parser("h"))
  |> should.equal(Ok(#("e", "")))
}

// Parser Combinator Tests
pub fn many_parser_test() {
  "hhhh"
  |> { char_parser("h") |> many }
  |> should.equal(Ok(#(["h", "h", "h", "h"], "")))
}

pub fn some_parser_test() {
  "ooh23"
  |> { char_parser("o") |> some }
  |> should.equal(Ok(#(["o", "o"], "h23")))
}

pub fn sequences_parser_test() {
  "he"
  |> sequences("he" |> string.split("") |> list.map(char_parser))
  |> should.equal(Ok(#(["h", "e"], "")))
}

pub fn then_keep_test() {
  "{ null  }"
  |> {
    char_parser("{")
    |> then_keep_right(null_parser())
    |> then_keep_left(char_parser("}"))
  }
  |> should.equal(Ok(#(JsonNull, "")))
}

pub fn sep_by_test() {
  "3, 4,       5  "
  |> { number_parser() |> sep_by(char_parser(",")) }
  |> should.equal(
    Ok(#([JsonNumber(3.0), JsonNumber(4.0), JsonNumber(5.0)], "")),
  )
}

// JSON Parser Tests
pub fn null_parser_test() {
  "null"
  |> null_parser()
  |> should.equal(Ok(#(JsonNull, "")))
}

pub fn bool_parser_test() {
  "false"
  |> bool_parser()
  |> should.equal(Ok(#(JsonBool(False), "")))
}

pub fn number_parser_test() {
  "123"
  |> number_parser()
  |> should.equal(Ok(#(JsonNumber(123.0), "")))

  ["3", "-3", "+3", " 34 ", "  +3  ", "  3.14 ", " +3.15", "  -3.15"]
  |> list.each(fn(str) { str |> number_parser() |> should.be_ok() })

  "1e1" |> number_parser() |> should.equal(Ok(#(JsonNumber(10.0), "")))
  "0.1e-1" |> number_parser() |> should.equal(Ok(#(JsonNumber(0.01), "")))
  // number too big, it will crash BEAM
  //"23456789012e66" |> number_parser() |> should.be_error
}

pub fn json_string_parser_test() {
  "\"hello\""
  |> json_string_parser()
  |> should.equal(Ok(#(JsonString("hello"), "")))

  "     \"\"       "
  |> json_string_parser()
  |> should.equal(Ok(#(JsonString(""), "")))
}

pub fn array_parser_test() {
  "[1, 2, 3]"
  |> array_parser()
  |> should.be_ok()
}

pub fn object_parser_test() {
  "{\"key\": \"value\"}"
  |> object_parser()
  |> should.be_ok()

  "{\"\": \"123\"}"
  |> object_parser()
  |> should.be_ok()
}

// Complex JSON Tests
pub fn nested_array_test() {
  "[1, [2, 3], [null]]"
  |> array_parser()
  |> should.be_ok()

  "[[]]" |> array_parser() |> should.be_ok()
}

pub fn nested_object_test() {
  "{\"a\": {\"b\": true}}"
  |> object_parser()
  |> should.be_ok()
}

pub fn recursive_test() {
  "[{}]" |> json_parser() |> should.be_ok

  "{\"object with 1 member\":[\"array with 1 element\"]}"
  |> json_parser()
  |> should.be_ok
}

pub fn escape_sequence_test() {
  "\"\\n\""
  |> json_string_parser()
  |> should.equal(Ok(#(JsonString("\n"), "")))

  "\"\\/\""
  |> json_string_parser()
  |> should.equal(Ok(#(JsonString("/"), "")))
}

pub fn unicode_escape_sequence_test() {
  "\"\\uCAFE\""
  |> json_string_parser()
  |> should.be_ok()

  "\\uCAFE\\uBABE\\uAB98\\uFCDE\\ubcda\\uef4A\\b\\f\\n\\r\\t"
  |> many(escape_sequence_parser())
  |> should.be_ok()
}

// File Parsing Test
pub fn parse_json_file_test() {
  parse_json_file("pass1.json")
  |> should.be_ok()
}
