

module Inner=CamlinternalFormatBasics
module U = Untyped.Cfmt

type 'a gkey = { id: 'a; ctx: string }
type dynkey = U.t gkey

include Map.Make(struct type t = dynkey let compare = compare end)

module W = Witness


let fmt (Inner.Format(c,_)) = c
let dynamic key =  U.Dyn (fmt key)
let make ?(ctx="") id = { id; ctx }


let box_then_add:
    ?ctx:string -> ('a,_,_,_,_,_) format6
    -> ('ab * 'a, unit * unit) W.l -> (W.fmta -> 'ab) -> W.box t -> W.box t =
    fun ?(ctx="") id spec f m ->
      add {id=dynamic id;ctx} (W.Box(spec,f)) m



let expand_then_add (type x y z l m)
    ?(ctx="")
    (id: (m,W.fmta,x,y,z,unit) format6)
    (spec: (l * m, unit * unit) W.l)
    (metafmt: l Metafmt.t) m =
  box_then_add ~ctx id spec
    Metafmt.(fun ppf -> expand spec @@ print ppf metafmt) m


let find m ppf ?(ctx="") id =
  let dynkey = { id = dynamic id; ctx } in
  let box = find dynkey m in
  let expected_spec = Conv.typer (fmt id) in
  W.unbox expected_spec box ppf

let add ?(ctx="") (Untyped.Dyn { spec; ref; fmt }) m =
  expand_then_add ~ctx ref spec fmt m
