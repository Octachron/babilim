

module Inner=CamlinternalFormatBasics
module U = Untyped.Cfmt

type 'a gkey = { id: 'a; ctx: string; num: int option }
type dynkey = U.t gkey

module M =  Map.Make(struct type t = dynkey let compare = compare end)
module W = Witness
type t = Metafmt.box M.t
let empty: t = M.empty

let fmt (Inner.Format(c,_)) = c
let dynamic key =  U.Dyn (fmt key)
let make ?num ?(ctx="") id = { id; ctx; num }


let box_then_add:
    ?num:int -> ?ctx:string -> ('a,_,_,_,_,_) format6
    -> ('ab * 'a, unit * unit) W.l -> 'ab Metafmt.t -> t -> t =
    fun ?num ?(ctx="") id spec f m ->
      M.add {id=dynamic id;ctx;num} (Metafmt.Box(spec,f)) m

let xprintf m ppf ?num ?(ctx="") id =
  let dynkey = { id = dynamic id; ctx; num } in
  let box = M.find dynkey m in
  let expected_spec = Conv.typer (fmt id) in
  Metafmt.unbox expected_spec box ppf

let add ?num ?(ctx="") (Untyped.Dyn { spec; ref; fmt }) m =
  box_then_add ?num ~ctx ref spec fmt m
