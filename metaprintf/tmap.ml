
module Inner=CamlinternalFormatBasics
module U = Untyped.Cfmt

type 'a gkey = { id: 'a; ctx: string; num: int option }
type dynkey = U.u gkey

module M =  Map.Make(struct type t = dynkey let compare = compare end)
module W = Witness

type t = Metafmt.Box.u M.t
type tmap = t
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
  Metafmt.Box.kfprintf k expected_spec box ppf

let xsprintf m ?num ?(ctx="") id =
  let dynkey = { id = dynamic id; ctx; num } in
  let box = M.find dynkey m in
  let expected_spec = Conv.typer (fmt id) in
  Metafmt.Box.sprintf expected_spec box

let add ?num ?(ctx="") (Untyped.Dyn { spec; ref; fmt }) m =
  box_then_add ?num ~ctx ref spec fmt m

module Store = struct
  let magic = "Babilim.Translation.Store"
  type nonrec t = { lang:string; plural:Math_expr.any; translations: t }
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

  let default =
    {
      kfprintf = Format.kfprintf;
      knfprintf = (fun k ppf n s pl ->
          if n = 1 then Format.kfprintf k ppf s
          else Format.kfprintf k ppf pl
        );

    }

  let from_map expr tmap =
    let kfprintf k ppf fmt =
      try xkfprintf tmap ?num:None ?ctx:None k ppf fmt  with
      | Not_found ->
        default.kfprintf k ppf fmt
    in
    let knfprintf k ppf num fmts fmtpl =
      let num = Math_expr.eval_int num expr in
      let CamlinternalFormatBasics.(Format(_,ctx)) = fmts in
      try xkfprintf tmap ~num ~ctx k ppf fmtpl with
      | Not_found -> default.knfprintf k ppf num fmts fmtpl
    in
    { kfprintf; knfprintf }


  let from_store f =
    match Store.read @@ f ^ ".bo" with
    | None -> None
    | Some m -> Some (from_map m.Store.plural m.Store.translations)
end
