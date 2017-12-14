type _ t =
  | And: bool t * bool t -> bool t
  | Or: bool t * bool t -> bool t
  | Greater: int t * int t -> bool t
  | Less: int t * int t -> bool t
  | GreaterEq: int t * int t -> bool t
  | LessEq: int t * int t -> bool t
  | Equal: int t * int t -> bool t
  | Not: bool t -> bool t

  | Literal: int -> int t
  | N: int t
  | Mod: int t * int t -> int t
  | If: bool t * int t * int t -> int t

type any = Any: _ t -> any [@@unboxed]

let int_of_bool b = if b then 1 else 0

let rec eval: type a. int -> a t -> a = fun n ->
  let eval x = eval n x in
  function
  | Literal x -> x
  | If(b,then',else') -> eval (if eval b then then' else else')
  | Greater(a,b)-> eval a > eval b
  | Less(a,b) -> eval a < eval b
  | GreaterEq(a,b)-> eval a >= eval b
  | LessEq(a,b) -> eval a <= eval b
  | Not a -> not (eval a)
  | And(x,y) -> eval x && eval y
  | Or(x,y) -> eval x || eval y
  | Equal(x,y) -> eval x = eval y
  | N -> n
  | Mod(x,y) -> eval x mod eval y

let eval_int n (Any x) =
  let eval x = eval n x in
  match x with
  | Literal x -> x
  | If _ as x  -> eval x
  | And _ as x -> int_of_bool (eval x)
  | Or _ as x ->  int_of_bool (eval x)
  | Not _ as x -> int_of_bool (eval x)
  | Equal _  as x -> int_of_bool (eval x)
  | N -> n
  | Mod _ as x -> eval x
  | Greater _ as x -> int_of_bool (eval x)
  | Less _ as x -> int_of_bool (eval x)
  | GreaterEq _ as x -> int_of_bool (eval x)
  | LessEq _ as x -> int_of_bool (eval x)
