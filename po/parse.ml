let file input =
  let ch = open_in input in
  let lex = Lexing.from_channel ch in
  Parser.file Lexer.main lex
