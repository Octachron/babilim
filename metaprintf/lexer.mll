{ open Parser }

let num = ['0'-'9']+
let letter = ['a'- 'z' 'A' - 'Z']
let integer_modifier = ['l' 'L' 'n']
let std = [^ '%' '@']

rule main = parse
  | "%" (num as n) "$" (integer_modifier? letter as l)
  { INDEXED(l, int_of_string n) }
  | "%" (integer_modifier? letter+ as l) { SIMPLE l }
  | "%%" { PERCENT }
  | "@@" { AT }
  | std+ as s { TEXT s }
  | eof { EOF }
