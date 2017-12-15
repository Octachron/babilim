(** Dynamicaly parse from strings *)

val fmt: string -> ('a,'b,'c) Untyped.Cfmt.t
(** Parse a format string *)

val metafmt : string -> ('a,'b,'c,'d,'e,'f) format6 ->
  ('g,'f,'b,'c) Untyped.dyn
  (** [metafmt src ref] Parse a metaformat string from [src],
      compatible with the format string [src] *)
