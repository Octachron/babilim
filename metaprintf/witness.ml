type fmta = Format.formatter

type 'a cp = (fmta -> 'a -> unit)
type show = Show: (fmta -> 'a -> unit) * 'a -> show

type ('x,'y,'p) a = <x:'l*'m; fx:(('x -> 'l) * ('y -> 'm))>
  constraint 'p = 'l * 'm
type ('x,'p) s = <x:'l*'m; fx:(('x -> 'l) * ('x -> 'm))>
  constraint 'p = 'l * 'm
type ('x,'y, 'p) d = <x:'l*'m; fx:(('x -> 'l) * ('y1 -> 'y2 -> 'm))>
  constraint 'p = 'l * 'm
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
(*  | Box: ('inside * 'more_inside, 'middle) l
          -> <x:'middle; fx:'inside * 'more_inside> arg*)

and (_,_) l =
  | []: ('tail * 'moretail ,'tail *'moretail) l
  | (::): <x:'l * 'm ; fx:'res * 'resm > arg * ('l * 'm, 'tail) l
    -> ('res * 'resm,'tail) l


type (_,_) eq = Eq: ('x,'x) eq
type dyn = Dyn: ('a * 'b,'c * 'd) l -> dyn

let rec (@): type ls l2 t. (ls, l2) l -> (l2,t) l -> (ls,t) l =
  fun l r ->
    match l with
    | [] -> r
    | a :: q -> a :: ( q @ r )

let eq_arg: type x left left_more right right_more.
  < x:x; fx:(left * left_more) > arg ->
  < x:x; fx:(right * right_more) > arg
  -> (left,right) eq option =
  fun x y ->
  match x, y with
  | Int, Int -> Some Eq
  | Float, Float -> Some Eq
  | A, A -> Some Eq
  | String, String -> Some Eq
  | _ -> None

let conv_arg: type x left right left_more right_more.
  <x:x; fx:left * left_more > arg -> <x:x; fx:right * right_more> arg  ->
  <x:x; fx:left * left_more > arg  option =
  fun x y ->
  match x, y with
  | Int, Int -> Some Int
  | Float, Float -> Some Float
  | A, A -> Some A
  | String, String -> Some String
  | _ -> None


let rec (===): type left left_more right right_more t.
  (left * left_more, t) l -> (right * right_more,t) l -> (left,right) eq option =
  fun x y ->
  match x, y with
  | [], [] -> Some Eq
  | [], _ :: _ -> None
  | _ :: _, [] -> None
  | Int :: l , Int :: r ->
    begin match l === r with
      | None -> None
      | Some Eq -> Some Eq
    end
  | A :: l , A :: r ->
    begin match l === r with
      | None -> None
      | Some Eq -> Some Eq
    end
  | Float :: l , Float :: r ->
    begin match l === r with
      | None -> None
      | Some Eq -> Some Eq
    end

  | String :: l , String :: r ->
    begin match l === r with
      | None -> None
      | Some Eq -> Some Eq
    end
  (*  | Box il :: l, Box ir :: r -> (il @ l ) === (ir @ r )*)
  | _ -> None


let rec (<:): type left left_more right right_more t t2.
  (left * left_more,t) l -> (right * right_more,t2) l ->
  (right * right_more, t2) l option = fun x y ->
  match x, y with
  | [], [] -> Some []
  | [], _ :: _ -> None
  | _ :: _, [] -> None
  | Int :: l , Int :: r ->
    begin match l <: r with
      | None -> None
      | Some r -> Some (Int :: r)
    end
  | A :: l , A :: r ->
    begin match l <: r with
      | None -> None
      | Some r -> Some (A :: r)
    end
  | Float :: l , Float :: r ->
    begin match l <: r with
      | None -> None
      | Some r -> Some (Float :: r)
    end

  | String :: l , String :: r ->
    begin match l <: r with
      | None -> None
      | Some r -> Some (String :: r )
    end
  (*  | Box x :: l, Box y :: r -> (x @ l ) <: ( y @ r )*)
  | _ -> None



let remember (type a b c d) (x:(a*b,c*d) l ) (Dyn y) =
  y <: x

let (<::)(Dyn l) x = l <: x

let l = [A;A;Int;Float]
(* let test = [A;A;Int;Float] === [A;A;Int;Float] (*weak*) *)
(* let retype = (Dyn l) <:: l (* weak *) *)

type box = Box: ('core * _, unit * _) l * 'core -> box




type (_,_) h = H: ('a * 'b,'c * 'd) l -> ('b,'d) h

let rec refresh: type a b c d e. (a, b * c) s arg ->
  (a, d * e) s arg
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
  (x, ll * lm) s arg -> (ll * lm, rf * r) l ->
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
(* | Box l :: q -> do_nothing (H l @/ H q) *)




let rec unbox: type a b c. (b, unit) h -> box -> b =
  fun (H spec) (Box(spec',f)) ->
    match spec, spec' with
    | [], [] -> ()
    | Char :: l, Char :: r ->
      fun (n:char) -> unbox (H l) (Box(r, f n))
    | Int :: l, Int :: r ->
      fun (n:int) -> unbox (H l) (Box(r, f n))
    | Float :: l, Float :: r ->
      fun (x:float) -> unbox (H l) (Box(r, f x))
    | A :: l, A :: r ->
      fun show x -> unbox (H l) (Box(r, f (Show(show,x))))
(*    | Box q :: l, r -> unbox (H q @/ H l) (Box(r,f))
      | l , Box b ::r -> unbox (H l) (Box(b@r,f)) *)
    | _ -> do_nothing (H spec)

let box =
  let f i (Show(f,x)) =
    Format.printf "i:%d" i;
    Format.printf "show:%a" f x in
  Box([Int;A],f)

;; unbox (H [Int;A]) box 6 Format.pp_print_string "Hi"
