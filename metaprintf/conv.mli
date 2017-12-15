(** Format string analysis *)

exception Unsupported of string
(** Nested format argument [%( %)], format printing
    [%{ %}] and custom printer are not supported *)

val typer : ('a,'b,'c,'d,'e,'f) CamlinternalFormatBasics.fmt
  -> ('a,'f,'g,'b,'c) Witness.h
(** Recompute a list of specificiers from a format string *)
