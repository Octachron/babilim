module W = Witness


open CamlinternalFormatBasics

let padding (type a b c d e) (y:(a,b) padding)
  : (a , b, c, d, e ) W.h  =
  match y with
    | No_padding -> H []
    | Lit_padding _ -> H []
    | Arg_padding _ -> H [S Int]

let pr (type a b c d e) (y:(a,b) precision)
  : (a, b, c,d, e) W.h  =
  match y with
    | No_precision -> H []
    | Lit_precision _ -> H []
    | Arg_precision -> H [S Int]

exception Unsupported of string
let unsupported s = raise (Unsupported s)
open W
let (@) = (@/)
let rec typer : type a b c d e f g. (a,b,c,d,e,f) fmt -> (a,f,g,b,c) W.h =
  fun fmt -> match fmt with
    | Char f -> Char <::> f
    | Caml_char f -> Char <::> f
    | String (p,f) -> (padding p) @ (String <::> f)
    | Caml_string (p,f) -> (padding p) @ (String <::> f)
    | Int (k,pa,pre,f) -> padding pa @ pr pre @ (Int <::> f)
    | Int32 (k,pa,pre,f) -> padding pa @ pr pre @ (Int32 <::> f)
    | Int64 (k,pa,pre,f) -> padding pa @ pr pre @ (Int64 <::> f)
    | Nativeint (k,pa,pre,f) -> padding pa @ pr pre @ (Nativeint <::> f)
    | Bool (pa,f) -> padding pa @ (Bool <::> f)
    | Float(_,pa,pre,f) -> padding pa @ pr pre @ (Float <::> f)

    | Flush f -> typer f
    | String_literal (_,f) -> typer f
    | Char_literal (_,f) -> typer f
    | Formatting_lit(_,f) -> typer f

    | Formatting_gen(f,f') -> boxes f @/ typer f'

    | End_of_format -> H []

    | Alpha f -> let W.H r = typer f in H (A :: r)
    | Theta f -> let W.H r = typer f in H( T :: r )

    | Format_subst _ -> unsupported "Format substitution %(fmt%)"
    | Format_arg _ -> unsupported "Format arg %{fmt%}"
    | Reader _ -> unsupported "Reader r"
    | Scan_char_set _ -> unsupported "Scanf char set […]"
    | Scan_get_counter _ -> unsupported "Scanf get_counter %n/%l/%N/µL"
    | Scan_next_char _ -> unsupported "Scanf next_char %0c"
    | Ignored_param _ -> unsupported "Scanf %_…"
    | Custom _ -> unsupported "Custom"

and boxes:  type a b c d e f g. (a,b,c,d,e,f) formatting_gen -> (a,f,g,b,c) W.h =
  function
  | Open_tag Format (f,_) -> typer f
  | Open_box Format (f,_) -> typer f


and (<::>): type b c x ll lm r d e f g.
  x s -> (r,b,c,d,e,f) fmt -> (x->r,f,g,b,c) h =
  fun x f -> let W.H r = typer f in H (S x :: r)
