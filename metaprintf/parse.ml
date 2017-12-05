let from_string f x = f Lexer.main (Lexing.from_string x)

let fmt x = from_string Parser.fmt x
let metafmt fmt = from_string Parser.metafmt @@ fmt
