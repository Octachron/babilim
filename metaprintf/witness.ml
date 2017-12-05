type fmta = Format.formatter

type (_,_) eq = Eq: ('x,'x) eq
type 'a cp = (fmta -> 'a -> unit)
type show = Show: (fmta -> 'a -> unit) * 'a -> show

type ('x,'l) exs = <x:'x; l:'l; fl:'x -> 'l>
type ('x,'y, 'l) d = <x:'x; l:'l; fl:'y1 -> 'y2 -> 'l>
  constraint 'y = 'y1 -> 'y2

type _ s =
  | Char : char s
  | Int: int s
  | Int32: int32 s
  | Int64: int64 s
  | Nativeint: nativeint s
  | Bool: bool s
  | Float: float s
  | String: string s
  | Theta: (fmta -> unit) s


let (===) (type a b) (x:a s) (y:b s): (a,b) eq option =
  match x, y with
  | Char, Char -> Some Eq
  | Int, Int -> Some Eq
  | Int32, Int32 -> Some Eq
  | Int64, Int64 -> Some Eq
  | Nativeint, Nativeint -> Some Eq
  | Bool, Bool -> Some Eq
  | Float, Float -> Some Eq
  | String, String -> Some Eq
  | Theta, Theta -> Some Eq
  | _ -> None


type _ arg =
  | S:  'x s -> ('x, _ )exs arg
  | A: (show, 'a cp -> 'a, _ ) d arg

and (_,_) l =
  | []: ('tail * 'moretail ,'tail *'moretail) l
  | (::): <x:'x ; l:'m; fl:'fm > arg * ('l * 'm, 'tail) l
    -> (('x -> 'l) *  'fm,'tail) l

let rec leq: type a b c d e f g h. (a * b, c * d) l -> (e * f, c * g) l ->
  (a,e) eq option =
  fun x y ->
    match x, y with
    | [], [] -> Some Eq
    | S x :: l, S y :: r ->
      begin match x === y with
        | None -> None
        | Some Eq -> match leq l r with
          | None -> None
          | Some Eq -> Some Eq
      end
    | A :: l, A :: r ->
      begin match leq l r with
        | None -> None
        | Some Eq -> Some Eq
      end
    | _ -> None


let canary = [S Int; S Int; A]
type dyn = Dyn: ('a * 'b,'c * 'd) l -> dyn

let rec (@): type ls l2 t. (ls, l2) l -> (l2,t) l -> (ls,t) l =
  fun l r ->
    match l with
    | [] -> r
    | a :: q -> a :: ( q @ r )

let l = [A;A; S Int; S Float]

type box = Box: ('core * _, unit * _) l * (fmta -> 'core) -> box

type (_,_) h = H: ('a * 'b,unit * 'd) l -> ('b,'d) h

let rec (@/): type ls l2 t. (ls, l2) h -> (l2,t) h -> (ls,t) h =
  fun (H x) (H y) -> match x with
    | [] -> H y
    | S x :: q -> single x q (H y)
    | (A :: q) ->
      let H r = ( H q @/ H y ) in
      H ( A :: r )
(*   | ( (Box b) :: q ) -> H (b @ q) @/ (H y) *)
and single: type x ll lm r rf t.
  x s -> (ll * lm, unit * r) l ->
  (r,t) h -> (x->lm,t) h = fun x q y ->
  let H r = H q @/ y in
  H (S x :: r)

let rec do_nothing: type a b c. (a , unit) h -> a = fun (H spec) ->
  match spec with
  | [] -> ()
  | S _ :: q -> fun _ -> do_nothing (H q)
  | A :: q -> fun _ _ -> do_nothing (H q)


let rec unbox: type a b c. (b, unit) h -> box -> fmta -> b =
  fun (H spec) (Box(spec',f)) ppf ->
    match spec, spec' with
    | [], [] -> f ppf
    | S x :: l, S y :: r ->
      begin match x === y with
        | None -> do_nothing (H spec)
        | Some Eq ->
          fun x -> unbox (H l) (Box(r,fun ppf -> f ppf x)) ppf
      end
    | A :: l, A :: r ->
      fun show x -> unbox (H l) (Box(r, fun ppf -> f ppf (Show(show,x)))) ppf
    | _ -> do_nothing (H spec)
