
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

let rec nth: type x fin list r. (x,list) index -> (list,r) L.t -> x =
  fun index l ->
    match index, l with
    | Z, L.(a :: _) -> a
    | S n, L.(_ :: q) -> nth n q
    | _, L.[] -> raise (Invalid_argument "Metafmt.nth")

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

module Modal = struct
type modifier = Plus | Space | Hash
type ext = Lit of int | Star

type t = {
  modifier:modifier option ;
  padding:ext option;
  precision:ext option;
  variant:string option;
}


let default =
  { modifier = None; padding = None; precision = None; variant = None }

let modifier = function
  | "" -> None
  | "+" -> Some Plus
  | " " -> Some Space
  | "#" -> Some Hash
  | _ -> None

let ext = function
  | "" -> None
  | "*" -> Some Star
  | x -> Some (Lit (int_of_string x))


let lexmodal ~m ~pa ~pr =
  {modifier=modifier m ;
   padding=ext pa;
   precision=ext pr;
   variant =  None
  }

end

module Formatting_box = struct
  type t = HV | V | HOV | C
  let to_string = function
    | V -> "v"
    | HOV -> "hov"
    | HV -> "hv"
    | C -> ""
end


type ('a,'driver,'mid) atom =
  | Text: string -> _ atom
  | Hole:
        Modal.t
      * <x:'x; fl:_; l:_;driver:'driver; mid:'mid > W.arg
      * ('x,'src) index -> ('src,'driver,'mid) atom
  | Break: { space:int; indent: int } -> _ atom
  | Open_box: { kind:Formatting_box.t; indent:int} -> _ atom
  | Open_tag: string -> _ atom
  | Close_box: _ atom
  | Close_tag: _ atom
  | Fullstop: _ atom
  | Newline: _ atom


type ('a,'driver,'mid) t =
  | []: _ t
  | (::): ('a,'driver,'mid) atom * ('a,'driver,'mid) t -> ('a,'driver,'mid) t

open Format

let print_elt (type x) ppf modal (w: x W.s) (x:x) = match w with
  | W.Int -> pp_print_int ppf x
  | W.Char -> pp_print_char ppf x
  | W.Bool -> pp_print_bool ppf x
  | W.Int32 -> Format.fprintf ppf "%ld" x
  | W.Int64 -> Format.fprintf ppf "%Ld" x
  | W.Nativeint -> Format.fprintf ppf "%nd" x
  | W.Float -> pp_print_float ppf x
  | W.String -> pp_print_string ppf x

let print_hole (type x l fl) ppf modal
    (w:<x:x; l:l; fl:fl; driver:Format.formatter; mid:unit > W.arg) (x:x) =
  match w with
  | W.A -> let W.Show(f,x) = x in (f ppf x:unit)
  | W.T -> (x ppf:unit)
  | W.(S w) -> print_elt ppf modal w x

let rec kprint: type l r. (formatter -> r) ->
  formatter -> (l,formatter,unit) t -> (l,r) L.t
  -> r  =
  fun k ppf x args ->
    match x with
    | [] -> k ppf
    | Text s :: q ->
      pp_print_string ppf s; kprint k ppf q args
    | Hole(modal,w,n) :: q ->
      print_hole ppf modal w (nth n args); kprint k ppf q args
    | Open_box {kind;indent} :: q ->
      Format.fprintf ppf "@[<%s%d>" (Formatting_box.to_string kind) indent;
      kprint k ppf q args
    | Open_tag s :: q -> Format.pp_open_tag ppf s; kprint k ppf q args
    | Close_box :: q ->
      Format.pp_close_box ppf (); kprint k ppf q args
    | Close_tag :: q -> Format.pp_close_tag ppf (); kprint k ppf q args
    | Break {space;indent} :: q -> Format.pp_print_break ppf space indent;
      kprint k ppf q args
    | Fullstop :: q -> Format.pp_print_flush ppf (); kprint k ppf q args
    | Newline :: q -> Format.pp_force_newline ppf (); kprint k ppf q args


let rec expand_full: type l m r r f mid. (l * m, r * r, f, mid ) W.l
  -> ((l,r) L.t -> r) -> m =
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


let int x = Hole(Modal.default, S Int, x)
let str x = Hole(Modal.default, S String, x)
let show x = Hole(Modal.default, A, x)

let _0 = Z
let _1 = S Z
let _2 = S _1
let _3 = S _2
let _4 = S _3


module Box = struct
  type ('fin, 'driver, 'mid) b =
    Box: ('core * _, 'fin * _, 'driver, 'mid) W.l
         * ('core,'driver,'mid) t -> ('fin,'driver,'mid) b

  type u = { u : 'fin 'driver 'mid.  ('fin, 'driver, 'mid) b } [@@unboxed]
  exception Metafmt_type_error

  let unsafe (b:('f,'driver,'mid) b): u = {u = Obj.magic b}

  let kprintf (type x y r)
      (k:Format.formatter -> r)
      (W.H spec:(x, r, r, Format.formatter,unit) W.h)
      { u = Box(spec',metafmt) }:
    Format.formatter -> x =
    match W.leq spec spec' with
    | Some W.Eq -> fun ppf -> expand_full spec @@ kprint k ppf metafmt
    | None -> raise Metafmt_type_error


  let fprintf spec = kprintf (fun _ -> ()) spec

  let sprintf spec u =
    let b = Buffer.create 10 in
    kprintf (fun _ -> Buffer.contents b) spec u (Format.formatter_of_buffer b)
(*
  let sprintf (type x y)
      (W.H spec:(x, string, string, unit,string) W.h)
      { u = Box(spec',metafmt) }: x =
    match W.leq spec spec' with
    | Some W.Eq -> expand_full spec @@ sprint (Buffer.create 20) metafmt
    | None -> raise Metafmt_type_error*)
end
