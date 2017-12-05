
type (_,_) eq = Eq: ('x,'x) eq
type ('a,'c,'driver) cp = ('driver -> 'a -> 'c)
type ('driver,'c) show = Show: ('driver -> 'a -> 'c) * 'a -> ('driver,'c) show

type ('x,'m) exs = <x:'x; l:'l; fl:'x -> 'l;
                    driver:'driver; mid:'c
                   >
  constraint 'm = 'l * 'driver * 'c

type 'm alpha =
  <x:('driver,'c) show; l:'l; fl:('driver -> 'y -> 'c) -> 'y -> 'l;
   driver: 'driver;
   mid:'c
  >
  constraint 'm = 'l * 'driver * 'y * 'c


type 'm theta = <x:('driver -> 'c as 't); l:'l; fl: 't -> 'l;driver: 'driver;
                 mid:'c
                >
  constraint 'm = 'l * 'driver * 'c


type _ s =
  | Char : char s
  | Int: int s
  | Int32: int32 s
  | Int64: int64 s
  | Nativeint: nativeint s
  | Bool: bool s
  | Float: float s
  | String: string s


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
  | _ -> None


type _ arg =
  | S:  'x s -> ('x, _ ) exs arg
  | A:  _ alpha arg
  | T: _ theta arg

and (_,_,_,_) l =
  | []: ('tail * 'moretail ,'tail *'moretail,'driver,'mid) l
  | (::): <x:'x ; l:'m; fl:'fm; driver:'driver; mid:'mid > arg *
          ('l * 'm, 'tail,'driver,'mid) l
    -> (('x -> 'l) *  'fm,'tail,'driver,'mid) l

let rec leq: type a b c d e f g h dr mid.
  (a * b, c * d, dr, mid) l -> (e * f, c * g, dr, mid) l ->
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
    | T :: l, T :: r ->
      begin match leq l r with
        | None -> None
        | Some Eq -> Some Eq
      end
    | _ -> None

let canary = [S Int; S Int; A]
type ('b,'d,'mid,'driver) dyn = Dyn: ('a * 'b,'c * 'd,'mid,'driver) l ->
  ('b,'d,'mid,'driver) dyn

let rec (@): type ls l2 m d t. (ls, l2,d,m) l -> (l2,t,d,m) l -> (ls,t,d,m) l =
  fun l r ->
    match l with
    | [] -> r
    | a :: q -> a :: ( q @ r )

let l = [A;A; S Int; S Float]

type ('r,'d,'m) box = Box: ('core * _, 'r * _, 'd,'m) l * ('d -> 'core) ->
  ('r,'d,'m) box

type (_,_,_,_,_) h = H: ('a * 'b,'c * 'd,'dr,'m) l -> ('b,'d,'c,'dr,'m) h

let rec (@/): type ls l2 m t x y z d. (ls, l2,x,d,m) h -> (l2,t,y,d,m) h
  -> (ls,t,y,d,m) h =
  fun (H x) (H y) -> match x with
    | [] -> H y
    | S x :: q -> single x q (H y)
    | (A :: q) ->
      let H r = ( H q @/ H y ) in
      H ( A :: r )
    | (T :: q) ->
      let H r = ( H q @/ H y ) in
      H ( T :: r )


and single: type x m ll lm r rf t tm t2 d.
  x s -> (ll * lm, tm * r,d,m) l ->
  (r,t,t2,d,m) h -> (x->lm,t,t2,d, m) h = fun x q y ->
  let H r = H q @/ y in
  H (S x :: r)

let rec return: type a b c r r2 d m . r -> (a , r , r2, d, m) h -> a =
  fun r (H spec) -> match spec with
  | [] -> r
  | S _ :: q -> fun _ -> return r (H q)
  | T :: q -> fun _ -> return r (H q)
  | A :: q -> fun _ _ -> return r (H q)

exception Unbox_error
let rec unbox: type a b m c r r2 d. (b, r, r2,d,m) h -> (r,d,m) box -> d -> b =
  fun (H spec) (Box(spec',f)) ppf ->
    match spec, spec' with
    | [], [] -> f ppf
    | S x :: l, S y :: r ->
      begin match x === y with
        | None -> raise Unbox_error
        | Some Eq ->
          fun x -> unbox (H l) (Box(r,fun ppf -> f ppf x)) ppf
      end
    | A :: l, A :: r ->
      fun show x -> unbox (H l) (Box(r, fun ppf -> f ppf (Show(show,x)))) ppf
    | _ -> raise Unbox_error
