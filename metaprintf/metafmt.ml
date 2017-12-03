
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

let rec nth: type x fin list. (x,list) index -> (list,empty) L.t -> x =
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

let rec indexed_apply: type a src b. a -> (a,src,b) Meta.t -> (src,empty) L.t
  -> b =
  fun f indices src ->
  match indices with
  | Meta.[] -> f
  | Meta.[n] -> f (nth n src)
  | Meta.(n :: q) -> indexed_apply (f (nth n src)) q src

let () =
  indexed_apply (Format.printf "%s %s %s %s %d") Meta.[Z;Z;Z;Z;S Z] L.[ "Hi"; 1 ]


module W = Witness
type 'a atom =
  | Text: string -> _ atom
  | Hole: <x:'x; fl:_; l:_ > W.arg * ('x,'src) index -> 'src atom

type 'a t =
  | []: _ t
  | (::): 'a atom * 'a t -> 'a t


open Format
let print_elt (type x l fl) ppf (w:<x:x; l:l; fl:fl > W.arg) (x:x) =
  match w with
  | W.A -> let W.Show(f,x) = x in f ppf x
  | W.Int -> pp_print_int ppf x
  | W.Char -> pp_print_char ppf x
  | W.Bool -> pp_print_bool ppf x
  | W.Int32 -> Format.fprintf ppf "%ld" x
  | W.Int64 -> Format.fprintf ppf "%Ld" x
  | W.Nativeint -> Format.fprintf ppf "%nd" x
  | W.Float -> pp_print_float ppf x
  | W.String -> pp_print_string ppf x
  | W.Theta -> x ppf

let rec print: type l. formatter -> l t -> (l,empty) L.t -> unit =
  fun ppf x args ->
    match x with
    | [] -> ()
    | Text s :: q ->
      pp_print_string ppf s; print ppf q args
    | Hole(w,n) :: q ->
      print_elt ppf w (nth n args); print ppf q args

let rec expand: type l m t. (l * m, unit * t ) W.l
  -> ((l,empty) L.t -> unit) -> l =
  fun spec f -> match spec with
    | W.(Int :: q)  ->
      fun n -> expand q (fun l -> f L.(n :: l) )
    | _ -> assert false

let int x = Hole(Int,x)
let str x = Hole(String,x)
let show x = Hole(A,x)

let _0 = Z
let _1 = S Z
let _2 = S _1
let _3 = S _2
let _4 = S _3

let f ppf =
  print ppf
    [Text "A text with a variable";
     int _0;
     Text "that appears";
     int _0;
     str _1]

let g = expand W.[Int;String] (f Format.std_formatter)
