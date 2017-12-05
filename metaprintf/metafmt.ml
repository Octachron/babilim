
type 'a eq = Eq: (int * int) eq
type empty = (int * char) eq


module L = struct
  type ('a,'res) t =
    | []: ('res, 'res) t
    | (::): 'a * ('list,'res) t  -> ('a -> 'list, 'res) t
end

type ('x,'list) index =
  | Z: ('x, 'x -> _ ) index
  | S: ('x,'list) index -> ('x, _ -> 'list) index

let rec nth: type x fin list. (x,list) index -> (list,unit) L.t -> x =
  fun index l ->
    match index, l with
    | Z, L.(a :: _) -> a
    | S n, L.(_ :: q) -> nth n q
    | _, L.[] -> .

let m = nth Z [0]
let k = nth (S Z) [0;"zero"]

module Meta = struct
  type ('indexed,'source,'final) t =
    | []: ('res,'source,'res) t
    | (::): ('x,'source) index * ('indexed,'source,'final) t ->
      ('x -> 'indexed,'source,'final) t

  let x = [Z;Z; S Z]
end

let rec apply: type a b. a -> (a,b) L.t -> b = fun f l ->
  match l with
  | L.[] -> f
  | L.[a] -> f a
  | L.(a :: q) -> apply (f a) q

let rec indexed_apply: type a src b. a -> (a,src,b) Meta.t -> (src,unit) L.t
  -> b =
  fun f indices src ->
  match indices with
  | Meta.[] -> f
  | Meta.[n] -> f (nth n src)
  | Meta.(n :: q) -> indexed_apply (f (nth n src)) q src


module W = Witness
type ('a,'driver,'mid) atom =
  | Text: string -> _ atom
  | Hole: <x:'x; fl:_; l:_;driver:'driver; mid:'mid > W.arg
          * ('x,'src) index -> ('src,'driver,'mid) atom

type ('a,'driver,'mid) t =
  | []: _ t
  | (::): ('a,'driver,'mid) atom * ('a,'driver,'mid) t -> ('a,'driver,'mid) t

open Format

let print_elt (type x) ppf (w: x W.s) (x:x) = match w with
  | W.Int -> pp_print_int ppf x
  | W.Char -> pp_print_char ppf x
  | W.Bool -> pp_print_bool ppf x
  | W.Int32 -> Format.fprintf ppf "%ld" x
  | W.Int64 -> Format.fprintf ppf "%Ld" x
  | W.Nativeint -> Format.fprintf ppf "%nd" x
  | W.Float -> pp_print_float ppf x
  | W.String -> pp_print_string ppf x

let print_hole (type x l fl) ppf
    (w:<x:x; l:l; fl:fl; driver:Format.formatter; mid:unit > W.arg) (x:x) =
  match w with
  | W.A -> let W.Show(f,x) = x in (f ppf x:unit)
  | W.T -> (x ppf:unit)
  | W.(S w) -> print_elt ppf w x

let rec print: type l. formatter -> (l,formatter,'c) t -> (l,unit) L.t -> unit =
  fun ppf x args ->
    match x with
    | [] -> ()
    | Text s :: q ->
      pp_print_string ppf s; print ppf q args
    | Hole(w,n) :: q ->
      print_hole ppf w (nth n args); print ppf q args


let sprint_elt (type x) (w: x W.s) (x:x) = match w with
  | W.Int -> string_of_int x
  | W.Char -> Format.sprintf "%c" x
  | W.Bool -> string_of_bool x
  | W.Int32 -> Int32.to_string  x
  | W.Int64 -> Int64.to_string  x
  | W.Nativeint -> Nativeint.to_string  x
  | W.Float -> string_of_float x
  | W.String -> x

let sprint_hole (type x l fl) ppf
    (w:<x:x; l:l; fl:fl; driver:Format.formatter; mid:string > W.arg) (x:x) =
  match w with
  | W.A -> let W.Show(f,x) = x in (f ppf x:string)
  | W.T -> (x ppf:string)
  | W.(S w) -> sprint_elt w x


let rec sprint: type l. formatter -> (l,formatter,'c) t -> (l,unit) L.t -> string =
  fun ppf x args ->
    match x with
    | [] -> ""
    | Text s :: q ->
      s ^ sprint ppf q args
    | Hole(w,n) :: q ->
      sprint_hole ppf w (nth n args) ^ sprint ppf q args

let rec expand_full: type l m. (l * m, unit * unit, Format.formatter, unit ) W.l
  -> ((l,unit) L.t -> unit) -> m =
  fun spec f -> match spec with
    | W.(S x :: q)  ->
      fun n -> expand_full q (fun l -> f L.(n :: l) )
    | W.(A :: q) ->
      fun show x -> expand_full q (fun l -> f L.(W.Show(show,x) :: l))
    | W.(T :: q) ->
      fun t -> expand_full q (fun l -> f L.(t :: l))
    | W.[] -> f []

let rec expand: type l m. (l * m, unit * unit, Format.formatter, unit ) W.l
  -> ((l,unit) L.t -> unit) -> l =
  fun spec f -> match spec with
    | W.(S x :: q)  ->
      fun n -> expand q (fun l -> f L.(n :: l) )
    | W.(A :: q) ->
      fun x -> expand q (fun l -> f L.(x :: l))
    | W.(T :: q)  ->
      fun t -> expand q (fun l -> f L.(t :: l) )
    | W.[] -> f []


let int x = Hole(S Int,x)
let str x = Hole(S String,x)
let show x = Hole(A,x)

let _0 = Z
let _1 = S Z
let _2 = S _1
let _3 = S _2
let _4 = S _3


type ('fin, 'driver, 'mid) box =
    Box: ('core * _, 'fin * _, 'driver, 'mid) W.l
         * ('core,'driver,'mid) t -> ('fin,'driver,'mid) box


exception Metafmt_type_error

let unbox (type x y) (W.H spec:(x, unit, unit, Format.formatter,unit) W.h)
    (Box(spec',metafmt)):
  Format.formatter -> x =
  match W.leq spec spec' with
  | Some W.Eq -> fun ppf -> expand_full spec @@ print ppf metafmt
  | None -> raise Metafmt_type_error
