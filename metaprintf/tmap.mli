(** Translation map *)


type t
type tmap = t
val empty: t

val box_then_add:
  ?num:int -> ?ctx:string -> ('a,_,_,_,_,_) format6 ->
  ('ab * 'a, unit * unit, Format.formatter, unit) Witness.l ->
  ('ab, Format.formatter, unit) Metafmt.t -> t -> t
(** [box_then_add ?num ?ctx fmt metafmt tmap] registers the
    translations metafmt for fmt. The [num] and [ctx]
    optional arguments precises respectively the grammatical number of
    the translation and its context *)

val xfprintf: t -> Format.formatter -> ?num:int -> ?ctx:string ->
  ('a,Format.formatter,unit) format -> 'a
(** [xfprintf tmap ppf ?num ?ctx fmt] uses the translation
    [tmap] to find a potential translation to [fmt] and print
    this translation with the same expected arguments than
    the original format
*)

val xkfprintf:
  t -> (Format.formatter -> 'r)  -> Format.formatter
  -> ?num:int -> ?ctx:string
  -> ('a, Format.formatter, unit, 'c, 'd, 'r) format6
  -> 'a
(** [xkfprintf tmap k ppf ?num ?ctx fmt] uses the translation
    [tmap] to find a potential translation to [fmt] and print
    this translation with the same expected arguments than
    the original format and applies the [k] to the formatter
    once the printing is finished
*)



val xsprintf:
  t -> ?num:int -> ?ctx:string
  -> ('a, Format.formatter, unit, 'c, 'd, string) format6
  -> 'a
(** [xsprintf tmap k ppf ?num ?ctx fmt] uses the translation
    [tmap] to find a potential translation to [fmt] and print
    this translation inside an internal buffer with the same expected arguments
    than the original format. Once the printing finished it outputs the
    printed string
*)



val add: ?num:int -> ?ctx:string
  -> (unit,unit,Format.formatter,unit) Untyped.dyn
  -> t
  -> t
(** [add ~num ~ctx (Dyn {ref;fmt;_}) tmap] registers the metafmt [fmt] as
    a translations for the reference format [ref]. The [num] and [ctx]
    argument precises respectively the grammatical number of the translation
    and its context *)

module Store: sig
  (** Storing translation maps *)

    type t = { lang:string; plural:Math_expr.any; translations: tmap }
  (** Stored translation: include the lang and the projection expression
      for computing grammatical number classes from a given integer *)

    val write: t -> string -> unit
    (** [write translations filename] *)

   val read: string -> t option
    (** [read filename] try to read the file [filename] and returns
        the stored translation info if successful *)

 end

module Implementation: sig
  (** Implementation function *)


  type t =
  {


    kfprintf:
      'a 'r. (Format.formatter -> 'r) -> Format.formatter
      -> ('a,Format.formatter,unit,'r) format4-> 'a;
    (** Singular form printing function *)

    knfprintf:
      'a 'r. (Format.formatter -> 'r) -> Format.formatter
      -> int -> (('a,Format.formatter,unit,'r) format4 as 'f)
      -> 'f -> 'a;
    (** Plural form printing function *)

  }

  val default: t
    (** Default implementation for singular form is
        [Format.kfprintf]. And the plural [knfprintf] is using
        the english/germanic distinction between singular and
        plural.
    *)

  val from_map: Math_expr.any -> tmap -> t
  (** Create an implementation from grammatical number classifier and
    a translation map *)

  val from_store: string -> t option
  (** [from_store filename] creates an implementation from a stored translation
      map *)

end
