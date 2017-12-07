

module Inner=CamlinternalFormatBasics
module U = Untyped.Cfmt

type 'a gkey = { id: 'a; ctx: string; num: int option }
type dynkey = U.u gkey

module M =  Map.Make(struct type t = dynkey let compare = compare end)
module W = Witness

type t = Metafmt.Box.u M.t
let empty: t = M.empty

let fmt (Inner.Format(c,_)) = c
let dynamic key =  U.unsafe (U.Dyn (fmt key))
let make ?num ?(ctx="") id = { id; ctx; num }


let box_then_add:
    ?num:int -> ?ctx:string -> ('a,_,_,_,_,_) format6
    -> ('ab * 'a, unit * unit, Format.formatter, unit) W.l ->
    ('ab, Format.formatter, unit) Metafmt.t -> t -> t =
    fun ?num ?(ctx="") id spec f m ->
      M.add {id=dynamic id;ctx;num} Metafmt.Box.(unsafe (Box(spec,f) ) ) m

let xfprintf m ppf ?num ?(ctx="") id =
  let dynkey = { id = dynamic id; ctx; num } in
  let box = M.find dynkey m in
  let expected_spec = Conv.typer (fmt id) in
  Metafmt.Box.fprintf expected_spec box ppf

let xkfprintf m k ppf ?num ?(ctx="") id =
  let dynkey = { id = dynamic id; ctx; num } in
  let box = M.find dynkey m in
  let expected_spec = Conv.typer (fmt id) in
  Metafmt.Box.kprintf k expected_spec box ppf

let xsprintf m ?num ?(ctx="") id =
  let dynkey = { id = dynamic id; ctx; num } in
  let box = M.find dynkey m in
  let expected_spec = Conv.typer (fmt id) in
  Metafmt.Box.sprintf expected_spec box

let add ?num ?(ctx="") (Untyped.Dyn { spec; ref; fmt }) m =
  box_then_add ?num ~ctx ref spec fmt m

module Store = struct
  let magic = "Babilim.Translation.Store"
  type nonrec t = { lang:string; translations: t }
  let write (map:t) s =
    let ch = open_out_bin s in
    output_string ch magic;
    output_value ch map

  let read s =
    let ch = open_in_bin s in
    if really_input_string ch (String.length magic) = magic then
      let map: t = input_value ch in
      Some map
    else
      None
end


module Implementation = struct

  type t =
    {
      str: string -> string;
      fprintf: 'a. Format.formatter -> ('a,Format.formatter,unit) format  -> 'a;
      fnprintf: 'a.
           Format.formatter
        -> int
        -> ('a,Format.formatter,unit) format
        -> ('a,Format.formatter,unit) format
        -> 'a;

      sprintf: 'a. ('a,Format.formatter,unit,string) format4 -> 'a;
      snprintf: 'a. int ->
        (('a,Format.formatter,unit,string) format4 as 'f)
        -> 'f  -> 'a
    }

  let default =
    {
      str = (fun id -> id);
      fprintf = Format.fprintf;
      sprintf = Format.asprintf;
      fnprintf = (fun ppf n s pl ->
          if n = 1 then Format.fprintf ppf s
          else Format.fprintf ppf pl
        );
      snprintf = (fun n s pl ->
          if n = 1 then Format.asprintf s
          else Format.asprintf pl
        )

    }

  let from_map tmap =
    let fprintf ppf fmt =
      try xfprintf tmap ?num:None ?ctx:None ppf fmt  with
      | Not_found -> default.fprintf ppf fmt
    in
    let sprintf fmt =
      try xsprintf tmap ?num:None ?ctx:None fmt with
      | Not_found -> default.sprintf fmt
    in
    let fnprintf ppf num fmts fmtpl =
      let CamlinternalFormatBasics.(Format(_,ctx)) = fmts in
      try xfprintf tmap ~num ~ctx ppf fmtpl with
      | Not_found -> default.fnprintf ppf num fmts fmtpl
    in
    let snprintf num fmts fmtpl =
      let CamlinternalFormatBasics.(Format(_,ctx)) = fmts in
      try xsprintf tmap ~num ~ctx fmtpl with
      | Not_found -> default.snprintf num fmts fmtpl
    in
    let to_fmt str = CamlinternalFormatBasics.(
        Format(String_literal(str,End_of_format), str) ) in
    let str x = sprintf (to_fmt x) in
    { str; fprintf; sprintf; fnprintf; snprintf }


  let from_store f =
    match Store.read @@ f ^ ".bo" with
    | None -> None
    | Some m -> Some (from_map m.Store.translations)
end
