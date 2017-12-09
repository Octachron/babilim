%token<string> TEXT
%token<Metafmt.Modal.t * string> SIMPLE
%token<Metafmt.Modal.t * string * int> INDEXED
%token AT
%token PERCENT
%token EOF
%token<string * int * int> BREAK
%token FULLSTOP
%token FORCED_NEWLINE
%token<Metafmt.Formatting_box.t * int> OPEN_BOX
%token<string> OPEN_TAG
%token CLOSE_BOX
%token CLOSE_TAG

%start <('a,'b,'c,'d,'e,'f) format6 -> ('fl, 'f, 'b, 'c) Untyped.u> metafmt
%start <('a,'b,'c) Untyped.Cfmt.t> fmt

%{ open Untyped
   module C = CamlinternalFormatBasics
%}

%%

metafmt:
  | l=metalist EOF { l 1 }

fmt:
  | l = slist EOF { l }

metalist:
| { fun n -> nil }
| x=indexed l=metalist { (fun n fmt -> hcons x (l (n+1) fmt)) }
| x = SIMPLE l = metalist { let m, x = x in
  fun n fmt -> hcons (m, arg x, integer n) (l (n+1) fmt) }
| t=textlike l=metalist { (fun n fmt -> cons t (l n fmt)) }

slist:
| { Cfmt.nil }
| x=SIMPLE r=slist { Cfmt.scons x r }
| t=text_elt r=slist { Cfmt.tcons t r }
| f=formatting_elt  r=slist { Cfmt.fcons f r }
| b=OPEN_BOX r = slist
  {
    let kind, indent = b in
    Cfmt.box_cons kind indent r
  }
| t=OPEN_TAG r = slist { Cfmt.tag_cons t r }

formatting_elt:
| k=BREAK { let s, x, y = k in C.Break(s,x,y)  }
| FULLSTOP { C.Flush_newline }
| FORCED_NEWLINE { C.Force_newline }
| CLOSE_BOX { C.Close_box }
| CLOSE_TAG { C.Close_tag }

indexed:
| x=INDEXED { let m, l, n = x in
  m, arg l, integer n
  }

textlike:
| x = text_elt { A (Text x) }
| FULLSTOP { A Fullstop }
| FORCED_NEWLINE { A Newline }
| x=OPEN_BOX { let kind, indent = x in A (Open_box{kind;indent}) }
| s=OPEN_TAG { A (Open_tag s) }
| CLOSE_BOX { A Close_box }
| CLOSE_TAG { A Close_tag }
| b= BREAK { let (_,x,y) = b in A (Break{space=x;indent=y}) }

text_elt:
  | AT { "@@" }
  | PERCENT { "%%" }
  | s = TEXT { s }
