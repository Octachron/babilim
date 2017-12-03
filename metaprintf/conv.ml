module W = Witness


open CamlinternalFormatBasics

let padding (type a b c d) (y:(a,b) padding)
  : (a , b ) W.h  =
  match y with
    | No_padding -> H []
    | Lit_padding _ -> H []
    | Arg_padding _ -> H [S Int]

let pr (type a b) (y:(a,b) precision)
  : (a, b) W.h  =
  match y with
    | No_precision -> H []
    | Lit_precision _ -> H []
    | Arg_precision -> H [S Int]

exception Unsupported of string
let unsupported s = raise (Unsupported s)
open W
let (@) = (@/)
let rec typer : type a c d e f. (a,fmta,unit,d,e,f) fmt -> (a,f) W.h =
  fun fmt -> match fmt with
    | Char f -> Char <::> f
    | Caml_char f -> Char <::> f
    | String (p,f) -> (padding p) @ (String <::> f)
    | Caml_string (p,f) -> (padding p) @ (String <::> f)
    | Int (k,pa,pre,f) -> padding pa @ pr pre @ (Int <::> f)
    | Int32 (k,pa,pre,f) -> padding pa @ pr pre @ (Int32 <::> f)
    | Int64 (k,pa,pre,f) -> padding pa @ pr pre @ (Int64 <::> f)
    | Nativeint (k,pa,pre,f) -> padding pa @ pr pre @ (Nativeint <::> f)
    | Bool f -> Bool <::> f
    | Float(_,pa,pre,f) -> padding pa @ pr pre @ (Float <::> f)
    | Theta f -> Theta <::> f

    | Flush f -> typer f
    | String_literal (_,f) -> typer f
    | Char_literal (_,f) -> typer f
    | Formatting_lit(_,f) -> typer f

    | Formatting_gen(f,f') -> boxes f @/ typer f'

    | End_of_format -> H []
    | Alpha f -> let W.H r = typer f in H (A :: r)

    | Format_subst _ -> unsupported "Format substitution %(fmt%)"
    | Format_arg _ -> unsupported "Format arg %{fmt%}"
    | Reader _ -> unsupported "Reader r"
    | Scan_char_set _ -> unsupported "Scanf char set […]"
    | Scan_get_counter _ -> unsupported "Scanf get_counter %n/%l/%N/µL"
    | Scan_next_char _ -> unsupported "Scanf next_char %0c"
    | Ignored_param _ -> unsupported "Scanf %_…"
    | Custom _ -> unsupported "Custom"

and boxes:  type a c d e f. (a,fmta,unit,d,e,f) formatting_gen -> (a,f) W.h =
  function
  | Open_tag Format (f,_) -> typer f
  | Open_box Format (f,_) -> typer f


and (<::>): type x ll lm r d e f.
  x s -> (r,fmta,unit,d,e,f) fmt -> (x->r,f) h =
  fun x f -> let W.H r = typer f in H (S x :: r)

type anyfmt = Any: _ format6 -> anyfmt [@@unboxed]

module Map = struct
  include Map.Make(struct type t = anyfmt let compare = compare end)

  let add:
    ('a,fmta,_,_,_,unit) format6
    -> ('ab * 'a, unit * unit) l -> (fmta -> 'ab) -> box t -> box t =
    fun fmt spec f m ->
      add (Any fmt) (Box(spec,f)) m

  let find (Format (c,_) as fmt) m =
    let box = find (Any fmt) m in
    let expected_spec = typer c in
    unbox expected_spec box

end



let meta_add (type x y z l m) (fmt: (m,fmta,x,y,z,unit) format6)
    (spec: (l * m, unit * unit) W.l)
    (metafmt: l Metafmt.t) m =
  Map.add fmt spec Metafmt.(fun ppf -> expand spec @@ print ppf metafmt) m
