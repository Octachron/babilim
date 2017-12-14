%token EQUAL
%token GREATER
%token LESS
%token GREATEREQ
%token LESSEQ
%token AND
%token OR
%token<int> LITERAL
%token N
%token MOD
%token NE
%token IF
%token THEN
%token ELSE
%token QUESTION
%token COLON
%token EOF
%token LP
%token RP

%nonassoc ELSE
%nonassoc COLON
%nonassoc MOD
%left OR
%left AND
%nonassoc EQUAL, NE, GREATER, LESS, LESSEQ, GREATEREQ

%{ open Metaprintf.Math_expr %}
%start<Metaprintf.Math_expr.any> expr
%%

expr:
  | e= int_expr EOF { Any e }
  | e=bool_expr EOF { Any e }

int_expr:
| n=LITERAL { Literal n }
| n=int_expr MOD k=int_expr { Mod(n,k) }
| N { N }
| IF test=bool_expr THEN th=int_expr ELSE el=int_expr
 { If(test,th,el) }
| test=bool_expr QUESTION th=int_expr COLON el=int_expr
 { If(test,th,el) }
| LP e=int_expr RP { e }

bool_expr:
|  x=int_expr GREATER y=int_expr { Greater(x,y) }
|  x=int_expr LESS y=int_expr { Less(x,y) }
|  x=int_expr GREATEREQ y=int_expr { GreaterEq(x,y) }
|  x=int_expr LESSEQ y=int_expr { LessEq(x,y) }
|  x=int_expr EQUAL y=int_expr { Equal(x,y) }
|  x=int_expr NE y=int_expr { Not(Equal(x,y)) }
|  x=bool_expr AND y=bool_expr { And(x,y) }
|  x=bool_expr OR y=bool_expr { Or(x,y) }
| LP e=bool_expr RP { e }
