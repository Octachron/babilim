
{
open Parser

let u x =
try Scanf.unescaped x with
| _ -> Format.eprintf "Unescaping failed on %s@." x; x

let make key s =
  let s = u s in
  match key with
  | "msgid" -> ID s
  | "msgid_plural" -> PLURAL s
  | "msgstr" -> STR s
  | "msgctxt" -> CTX s
  | _ -> ERROR

let pmake key s = match key with
  | "msgid" -> PREV_ID (u s)
  | "msgid_plural" -> PREV_PLURAL (u s)
  | _ -> ERROR


}

let newline = ('\r'* '\n')+
let line = ([^ '\n' '\r'])*
let space = ['\t' ' ']*
let num = ['0'-'9']+
let key = ['a' - 'z' 'A' - 'Z' '_']+

rule main = parse
  | newline { main lexbuf }
  | "# " space (line as text) space newline { TCOMMENT (text) }
  | "#." space (line as text) space newline { PCOMMENT (text) }
  | "#," space (line as text) space newline { FLAG (text) }
  | "#:" space (line as text) ":" (num as n) space newline
    {  LOC { file=text; line=int_of_string n } }
  | space (key as key) space "\"" (line as q) "\"" space newline
    {make key q }
  | space "msgstr[" (num as n) "]" space "\"" (line as q) "\"" space newline
    { STRN (int_of_string n,u q) }
  | space "\"" (line as l) "\"" space newline { MORESTR (u l)}
  | "#|" space (key as key) space "\"" (line as l) "\"" newline { pmake key l }
  | "#~" line newline { main lexbuf }
  | eof { EOF }
