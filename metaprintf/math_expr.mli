(** Math expression for plural form

    The notion of singular and plural varies across languages.
    For instance, French consider that any number [n>1] should be plural and
    [0] or [1] is singular. Contrarily, English only uses plural for [n=1].
    Similarly dual (aka a special case for [n=2]) is relatively common across
    languages, and Czech (and other slavic language like Polish)
    makes a grammatical distinction between [n=1],[n=2],[n=3],[n=4]
    and [n=5+].

    The mini math interpreter is then used to project a number
    [n] to the right grammatical category.

*)

(** Math expression *)
type 'a t =
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

(** Boxed math expression *)
type any = Any: _ t -> any [@@unboxed]


val eval_int: int -> any -> int
(** [eval_int n expr] evals the value of the expression for [n],
    converting bool to integer using the C-convention [true=1]
    and [false=0] *)
