type fmta = Format.formatter

type 'a cp = (fmta -> 'a -> unit)
type show = Show: (fmta -> 'a -> unit) * 'a -> show

type ('x,'l) s = <x:'x; l:'l; fl:'x -> 'l>
type ('x,'y, 'l) d = <x:'x; l:'l; fl:'y1 -> 'y2 -> 'l>
  constraint 'y = 'y1 -> 'y2

type _ arg =
  | Char:  (char, _ ) s arg
  | Int: (int, _ ) s arg
  | Int32: (int32, _ ) s arg
  | Int64: (int64, _ ) s arg
  | Nativeint: (nativeint, _ ) s arg
  | Bool: (bool, _ ) s arg
  | Float: (float, _ ) s arg
  | String: (string, _) s arg
  | Theta: ((fmta -> unit),_) s arg
  | A: (show, 'a cp -> 'a, _ ) d arg

and (_,_) l =
  | []: ('tail * 'moretail ,'tail *'moretail) l
  | (::): <x:'x ; l:'m; fl:'fm > arg * ('l * 'm, 'tail) l
    -> (('x -> 'l) *  'fm,'tail) l


let x= [Int; Int; A]
type (_,_) eq = Eq: ('x,'x) eq
type dyn = Dyn: ('a * 'b,'c * 'd) l -> dyn

let rec (@): type ls l2 t. (ls, l2) l -> (l2,t) l -> (ls,t) l =
  fun l r ->
    match l with
    | [] -> r
    | a :: q -> a :: ( q @ r )

let l = [A;A;Int;Float]
(* let test = [A;A;Int;Float] === [A;A;Int;Float] (*weak*) *)
(* let retype = (Dyn l) <:: l (* weak *) *)

type box = Box: ('core * _, unit * _) l * (fmta -> 'core) -> box




type (_,_) h = H: ('a * 'b,'c * 'd) l -> ('b,'d) h

let rec refresh: type a b c d e. (a, b) s arg ->
  (a, d) s arg
  = function
  | Int -> Int
  | Float -> Float
  | Char -> Char
  | Int32 -> Int32
  | Int64 -> Int64
  | Bool -> Bool
  | Nativeint -> Nativeint
  | String -> String
  | Theta -> Theta

let rec (@/): type ls l2 t. (ls, l2) h -> (l2,t) h -> (ls,t) h =
  fun (H x) (H y) -> match x with
    | [] -> H y
    | Char :: q -> single Char q (H y)
    | Int :: q -> single Int q (H y)
    | Int32 :: q -> single Int32 q (H y)
    | Int64 :: q -> single Int64 q (H y)
    | Nativeint :: q -> single Nativeint q (H y)
    | Float :: q -> single Float q (H y)
    | Bool :: q -> single Bool q (H y)
    | String :: q -> single String q (H y)
    | Theta :: q  -> single Theta q (H y)
    | (A :: q) ->
      let H r = ( H q @/ H y ) in
      H ( A :: r )
(*   | ( (Box b) :: q ) -> H (b @ q) @/ (H y) *)
and single: type x ll lm r rf t.
  (x, lm) s arg -> (ll * lm, rf * r) l ->
  (r,t) h -> (x->lm,t) h = fun x q y ->
  let H r = H q @/ y in
  H ( refresh x :: r)

let rec do_nothing: type a b c. (a , unit) h -> a = fun (H spec) ->
  match spec with
  | [] -> ()
  | Char :: q -> fun _ -> do_nothing (H q)
  | Int :: q -> fun _ -> do_nothing (H q)
  | Int32 :: q -> fun _ -> do_nothing (H q)
  | Int64 :: q -> fun _ -> do_nothing (H q)
  | Nativeint :: q -> fun _ -> do_nothing (H q)
  | Bool :: q -> fun _ -> do_nothing (H q)
  | Float :: q -> fun _ -> do_nothing (H q)
  | String :: q -> fun _ -> do_nothing (H q)
  | A :: q -> fun _ _ -> do_nothing (H q)
  | Theta :: q -> fun _ -> do_nothing (H q)


let rec unbox: type a b c. (b, unit) h -> box -> fmta -> b =
  fun (H spec) (Box(spec',f)) ppf ->
    match spec, spec' with
    | [], [] -> f ppf
    | Char :: l, Char :: r ->
      fun (n:char) -> unbox (H l) (Box(r,fun ppf -> f ppf n)) ppf
    | Int :: l, Int :: r ->
      fun (n:int) -> unbox (H l) (Box(r, fun ppf ->f ppf n)) ppf
    | Float :: l, Float :: r ->
      fun (x:float) -> unbox (H l) (Box(r, fun ppf -> f ppf x)) ppf
    | A :: l, A :: r ->
      fun show x -> unbox (H l) (Box(r, fun ppf -> f ppf (Show(show,x)))) ppf
    | _ -> do_nothing (H spec)

let box =
  let f ppf i (Show(f,x)) =
    Format.fprintf ppf "i:%d" i;
    Format.fprintf ppf "show:%a" f x in
  Box([Int;A],f)

;; unbox (H [Int;A]) box Format.std_formatter 6 Format.pp_print_string "Hi"
