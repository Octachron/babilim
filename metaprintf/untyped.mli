(** Dynamically typed parsers

    Parse dynamically format strings and metafmt strings from
    untyped strings.
*)


exception Dynamic_type_error of string
(** Dynamic type means potential runtime errors *)

module Cfmt: sig
  (** Dynamic parsing of format strings *)

    type ('fmter,'mid,'fin) t =
      Dyn: ('a,'fmter,'mid,'d,'e,'fin) CamlinternalFormatBasics.fmt ->
      ('fmter,'mid,'fin) t (**)
    (** Forget the type of specifiers *)

  type u =
    { u: 'fmter 'mid 'fin.  ('fmter,'mid,'fin) t } [@@unboxed]
    (** Universally quantify over formatter, result and intermediary types *)


  val unsafe: ('a,'b,'c) t -> u
  (** shortcut *)

  val nil: ('a,'b,'c) t
  (** Empty format string *)

  val scons: ('any * string) -> ('a,'b,'c) t -> ('a,'b,'c) t
  (** [cons (_,spec) fmt] adds a specifier to the format string [fmt] *)

  val tcons: string -> ('a,'b,'c) t -> ('a,'b,'c) t
  (** [tcons text fmt] adds a text element to the format string [fmt] *)

  val fcons:
    CamlinternalFormatBasics.formatting_lit -> ('a,'b,'c) t -> ('a,'b,'c) t
  (** [tcons flit fmt] adds a formatting element to the format string [fmt] *)

  val box_cons: Metafmt.Formatting_box.t -> int -> ('a,'b,'c) t -> ('a,'b,'c) t
  (** [box_cons kind indent fmt] open a formatting box [@[<kind indent]
      inside the format string [fmt] *)

    val tag_cons: string -> ('a,'b,'c) t -> ('a,'b,'c) t
  (** [tag_cons text fmt] open a tag [text] inside the format string [fmt] *)


end


(** {1 Main types } *)

type ('finl, 'finr, 'fmter, 'mid ) dyn = Dyn: {
    spec:('a * 'm , 'finl * 'finr, 'fmter, 'mid) Witness.l;
    ref: ('m,'fmter,'mid,'d,'e,'finr) format6;
    fmt:('a,'fmter, 'mid) Metafmt.t}  ->
    ('finl, 'finr, 'fmter, 'mid )
    dyn
(** [Dyn {fmt;spec;ref} Dynamically typed metafmt [fmt] expected to respect
    the specifier list [spec] extracted from the format string [ref]
*)

type ('driver,'mid) atom = A: ('a,'driver,'mid) Metafmt.atom ->
  ('driver, 'mid) atom [@@unboxed]
(** Dynamically typed atom *)

type nat = I: ('x,'src) Metafmt.index -> nat [@@unboxed]
(** Dynamically typed index *)


type ('d,'mid) arg = W : <x:'x; fl:_; l:_; mid:'mid; driver:'d > Witness.arg ->
  ('d,'mid) arg [@@unboxed]
(** Dynamically argument atom *)


exception Unsupported of string

val nil: ('a,'b,'c,'d,'e,'f) format6 -> ('g,'f,'b,'c) dyn
(** [nil fmt] creates an empty metafmt, expected to be compatible with
    the format [fmt] *)

val cons: ('d,'mid) atom -> ('finl, 'finr, 'd, 'mid) dyn
  -> ('finl, 'finr, 'd, 'mid) dyn
(** [cons atom metafmt] appends [atom] to the metafmt string *)

val hcons: Metafmt.Modal.t * ('a,'b) arg * nat ->
  ('finl,'finr,'a,'b) dyn -> ('finl,'finr,'a,'b) dyn
(** [hcons (m,arg,n) metafmt] appends the hole [Hole(m,arg,n) ] to the
    metafmt string *)

val integer: int -> nat
(** [integer n] computes the dynamically typed index [n] *)

val arg: string -> ('a,'b) arg
(** [arg s] computes the dynamically typed arg *)
