let from_string f x = f Lexer.main (Lexing.from_string x)

let fmt = from_string Parser.fmt
let metafmt fmt = from_string Parser.metafmt @@ fmt
