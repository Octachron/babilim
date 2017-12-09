{ open Parser

  let box_kind =
  let open Metafmt.Formatting_box in
  function
  | Some "v" -> V
  | Some "hv" -> HV
  | Some "hov" -> HOV
  | None -> C
  | _ -> failwith "Unknown box kind"

  let int = function
  | None -> 0
  | Some s -> int_of_string s

let semicolon s x y = match x, y with
| Some (x) ,Some(y) -> BREAK (s,int_of_string x, int_of_string y)
| _ -> BREAK(s,1,0)

let lexmodal = Metafmt.Modal.lexmodal
}

let num = ['0'-'9']+
let letter = ['a'- 'z' 'A' - 'Z']
let padding = num | "*"
let precision = "." (num|"*")
let modifier = ['+' ' ' '#']
let integer_modifier = ['l' 'L' 'n']
let std = [^ '%' '@']
let space = " "*

rule main = parse
  | "%" (num as n) "$" (modifier? as m) (padding? as pa) (precision? as pr)
  (integer_modifier? letter as l)
  { INDEXED(lexmodal ~m ~pa ~pr, l, int_of_string n) }
  | "%"  (modifier? as m) (padding? as pa) (precision? as pr)
  (integer_modifier? letter+ as l)
  { SIMPLE (lexmodal ~m ~pa ~pr,l) }
  | "%%" { PERCENT }
  | "@@" { AT }
  | "@ " as s { BREAK(s,1,0) }
  | "@," as s { BREAK(s,0,0) }
  | "@;"("<" space (num as x) space (num as y) space ">")? as s { semicolon s x y }
  | "@." { FULLSTOP }
  | "@\\n" { FORCED_NEWLINE }
  | "@[" ("<"space (letter+as kind) space (num as indent)? space ">")?
  { OPEN_BOX(box_kind kind, int indent) }
  | "@{" ("<" space (letter+ as label) space ">") { OPEN_TAG label }
  | "@}" { CLOSE_TAG }
  | "@]" { CLOSE_BOX }
  | std+ as s { TEXT s }
  | eof { EOF }
