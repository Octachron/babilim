%token<string> TEXT
%token<string> SIMPLE
%token<string * int> INDEXED
%token AT
%token PERCENT
%token EOF


%start <('a,Format.formatter,unit,'d,'e,unit) format6 -> Untyped.u> metafmt
%start <Untyped.Cfmt.t> fmt

%{ open Untyped %}

%%

metafmt:
  | EOF { nil }
  | t=text EOF { fun fmt -> cons t (nil fmt) }
  | t=text alt=alt_meta EOF { fun fmt -> cons t @@ alt 0 fmt }
  | alt = alt_meta EOF { alt 0 }

fmt:
  | EOF { Cfmt.nil }
  | t=stext EOF {Cfmt.(tcons t nil) }
  | t=stext alt=alt_simple EOF { Cfmt.tcons t alt }
  | alt = alt_simple EOF { alt }

alt_meta:
| { fun n -> nil }
| x=indexed l=alt_meta { (fun n fmt -> hcons x (l (n+1) fmt)) }
| x = SIMPLE l = alt_meta {
  fun n fmt -> hcons (arg x, integer n) (l (n+1) fmt) }
| t=text l=alt_meta { (fun n fmt -> cons t (l n fmt)) }

alt_simple:
| { Cfmt.nil }
| x=SIMPLE r=alt_simple { Cfmt.scons x r }
| t=stext r=alt_simple { Cfmt.tcons t r }

indexed:
| x=INDEXED { let l, n = x in
  arg l, integer n
  }

simple:
| x=SIMPLE { arg x }

stext:
| l =nonempty_list(text_elt) { String.concat "" l }

text:
| l=nonempty_list(text_elt) { A(Text(String.concat "" l)) }


text_elt:
  | AT { "@@" }
  | PERCENT { "%%" }
  | s = TEXT { s }
