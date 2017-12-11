(** Metafmt is a Format overlay that implements positional format specifier.

    For instance the format string ["%2$s %1$s %2$s"] requires two strings
    arguments and print the second string twice.

    Currently, this comes at the cost of advanced features of Format:
    subformat nesting ["%( %)"], user-provided padding and precision
    ["%*.*d"], format printing ["%{ %}"] and nested format inside tag and
    box specification ["@{<@[<%s>%s@]>"].

*)

(** empty type *)
type empty = (int, char) Witness.eq

(** {1 Heterogeneous list} *)

module L: sig
  type ('a,'res) t =
    | []: ('res, 'res) t
    | (::): 'a * ('list,'res) t  -> ('a -> 'list, 'res) t
  type 'a fix = ('a,empty) t
end

val apply: 'f -> ('f,'result) L.t -> 'result
(** [apply f [x_1;…;x_n]] computes [f x_1 … x_n] *)

type ('x,'list) index =
  | Z: ('x, 'x -> _ ) index
  | S: ('x,'list) index -> ('x, _ -> 'list) index
(** The unary integer, n= S (…(Z)…), select the type of the n-th element
    of the heterogeneous list L.t *)

val nth:  ('x,'list) index -> ('list,'r) L.t -> 'x
(** [nth k l] returns the k-th element of the list.
    Raise [Invalid_argument] if the index is greater
    than the list length. Note that this cannot happen
    if the type of the list is fixed to [L.fix] .
 *)

(** Heterogeneous list of indices of an heterogeneous list *)
module Meta: sig
  type ('indexed,'source,'final) t =
    | []: ('res,'source,'res) t
    | (::): ('x,'source) index * ('indexed,'source,'final) t ->
      ('x -> 'indexed,'source,'final) t
end

val indexed_apply:
  'f -> ('f,'arguments,'result) Meta.t -> 'arguments L.fix -> 'result
  (** [f [k_1;…;k_l] [x_1;…;x_n]] computes [f x_{k_1} x_{k_2} … x_{k_l}].
      Note that this functions will not type-check if [max_l k_l > n] *)

(** {1 Auxiliary types } *)
module Modal: sig
  (** Modal modifier to the specifier *)

  type modifier = Plus | Space | Hash
  type ext = Lit of int | Star

  type t = {
    modifier:modifier option ;
    padding:ext option;
    precision:ext option;
    variant:string option;
  }

  val default:t

  val modifier: string -> modifier option
  val ext: string -> ext option

  val lexmodal: m:string -> pa:string -> pr:string -> t
end

module Formatting_box: sig

  (** Formatting boxes *)
  type t = HV | V | HOV | C

  val to_string: t -> string

end

(** {1 Format atom} *)
type ('a,'driver,'mid) atom =
  | Text: string -> _ atom
  | Hole:
        Modal.t
      * <x:'x; fl:_; l:_;driver:'driver; mid:'mid > Witness.arg
      * ('x,'src) index -> ('src,'driver,'mid) atom
  | Break: { space:int; indent: int } -> _ atom
  | Open_box: { kind:Formatting_box.t; indent:int} -> _ atom
  | Open_tag: string -> _ atom
  | Close_box: _ atom
  | Close_tag: _ atom
  | Flush: _ atom
  | Fullstop: _ atom
  | Newline: _ atom

(** Main type *)
type ('a,'driver,'mid) t =
  | []: _ t
  | (::): ('a,'driver,'mid) atom * ('a,'driver,'mid) t -> ('a,'driver,'mid) t


val kfprintf: (Format.formatter -> 'result) -> Format.formatter
  -> ('args, Format.formatter, unit) t -> ('args,'result) L.t -> 'result
(** kfprintf with arguments boxed inside a heterogeneous list *)


val expand_full:
  ('metafmt_args * 'format_args, 'result * 'result, 'formatter, 'mid) Witness.l
  -> ( ('metafmt_args,'result) L.t -> 'result ) -> 'format_args
(** [expand_full spec f] takes an uncurried function [f]
    as argument and returns a curryied function with the format calling
    convention *)

val expand:
  ('metafmt_args * 'format_args, 'result * 'result, 'formatter, 'mid) Witness.l
  -> ( ('metafmt_args,'result) L.t -> 'result ) -> 'metafmt_args
(** [expand spec f] takes an uncurried function [f]
    as argument and returns a curryied function with the metafmt calling
    convention *)

val int: (int,'l) index -> ('l,_,_) atom
val float: (float,'l) index -> ('l,_,_) atom
val str: (string,'l) index -> ('l,_,_) atom
val show: ( ('a,'b) Witness.show,'l) index -> ('l,'a,'b) atom

val _0: ('x, 'x -> 'rest) index
val _1: ( 'x, 'first -> 'x -> 'rest) index
val _2: ( 'x, 'first -> 'second -> 'x -> 'rest) index
val _3: ( 'x, 'first -> 'second -> 'third -> 'x -> 'rest) index
val _4: ( 'x, 'first -> 'second -> 'third -> 'fourth -> 'x -> 'rest) index



module Box: sig
  (** Metafmt format specifier boxed together with a type witness *)

  type ('fin, 'driver, 'mid) b =
      Box: ('core * _, 'fin * _, 'driver, 'mid) Witness.l
           * ('core,'driver,'mid) t -> ('fin,'driver,'mid) b
    (** This type forget the argument used by the metafmt format *)

  type u = { u : 'fin 'driver 'mid.  ('fin, 'driver, 'mid) b } [@@unboxed]
   (** This type adds an universal quantification on the result,
       intermediary result and formatter type *)

  (** A shortcut avoiding the need to prove the fact that
      box types can always be universally quatified *)
  val unsafe: ('fin,'driver,'mid) b -> u

  exception Metafmt_type_error

  val kfprintf: (Format.formatter -> 'r)
    -> ('x, 'r, 'r, Format.formatter,unit) Witness.h
    -> u
    -> Format.formatter
    -> 'x
    (** [kfprintf k spec box ppf] checks that the boxed type witness
        and the type witness provided as an argument are compatible and then call
        the unboxed version of [kfprintf] if they are or
        raise [Metafmt_type_error] otherwise
    *)


  val fprintf:
    ('x, unit, unit, Format.formatter,unit) Witness.h
    -> u
    -> Format.formatter
    -> 'x

  val sprintf:
    ('x, string, string, Format.formatter,unit) Witness.h
    -> u -> 'x

end
