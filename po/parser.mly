
%token<string> ID
%token<string> STR
%token<string> CTX
%token<string> FLAG
%token<string> TCOMMENT
%token<string> PCOMMENT
%token<string> PLURAL
%token<int * string> STRN
%token<string> MORESTR
%token<Types.loc> LOC
%token ERROR
%token EOF
%token<string> PREV_ID
%token<string> PREV_PLURAL

%{ open Types %}

%start <Types.po> file

%%

file2:
  | ID MORESTR MORESTR EOF { of_list [] }

file:
  | header=msg r=list(entry) EOF {
  make header (of_list r)
  }

entry:
  | tc=list(TCOMMENT) pc=list(PCOMMENT) flgs=list(FLAG)
    r=LOC context=context
    flgs2=list(FLAG)
    msg=msg {
  {
    comments={programmer=pc;translator=tc};
    location = r;
    flags = flgs @ flgs2 ;
    context;
    previous=None;
    msg;
   }
}


%inline morestr(PRE):
 | s=PRE l=list(MORESTR) { s::l }

msg:
  | l=morestr(ID) f=cmsg { f l }

context:
  | { [] }
  | l=morestr(CTX){ l }

cmsg:
  | plural=morestr(PLURAL) translations=list(strp)
    { fun id -> Plural{id;plural;translations} }
  | translation = morestr(STR)
  { fun id -> Singular{id;translation} }

strp:
  ns=STRN sm=list(MORESTR) { let n, s = ns in (n,s::sm)  }

previous:
  | { }
