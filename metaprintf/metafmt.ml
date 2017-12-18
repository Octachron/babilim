
type empty = (int, char) Witness.eq


module L = struct
  type ('a,'res) t =
    | []: ('res, 'res) t
    | (::): 'a * ('list,'res) t  -> ('a -> 'list, 'res) t

  type 'a fix = ('a,empty) t
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

let rec indexed_apply: type a src b. a -> (a,src,b) Meta.t -> src L.fix
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


exception Unsupported of string
let unsupported x = raise (Unsupported x)

let ext = function
  | "" -> None
  | "*" -> Some Star
  | x -> try Some (Lit (int_of_string x)) with _ -> None


let lexmodal ~m ~pa ~pr l =
  {modifier=modifier m ;
   padding=ext pa;
   precision=(match pr with None -> None | Some pr -> ext pr);
   variant = Some l
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
      { modal: Modal.t;
        arg: <x:'x; fl:_; l:_;driver:'driver; mid:'mid > Witness.arg;
        pos: ('x,'src) index
      } -> ('src,'driver,'mid) atom
  | Break: { space:int; indent: int } -> _ atom
  | Open_box: { kind:Formatting_box.t; indent:int} -> _ atom
  | Open_tag: string -> _ atom
  | Close_box: _ atom
  | Close_tag: _ atom
  | Flush: _ atom
  | Fullstop: _ atom
  | Newline: _ atom


type ('a,'driver,'mid) t =
  | []: _ t
  | (::): ('a,'driver,'mid) atom * ('a,'driver,'mid) t -> ('a,'driver,'mid) t


module I = CamlinternalFormatBasics
open Format


let print_int (params: _ list) f ppf modal n =
  let open Modal in
  let padding, params =
    match modal.padding, params with
    | None, _ -> I.No_padding, params
    | Some (Lit k), _ -> I.Lit_padding (Right,k), params
    | Some Star, a :: q ->
      I.Lit_padding(Right,a), q
    | Some Star, [] -> assert false
  in
  let precision = match modal.precision, params with
    | None, _ -> I.No_precision
    | Some(Lit k), _ -> I.Lit_precision k
    | Some Star, a :: _ -> I.Lit_precision a
    | Some Star, [] -> assert false in
  let variant =
    let open I in
    match modal.modifier with
    | None -> begin
        match modal.variant with
        | None -> Int_d
        | Some x -> begin
            match x with
            | "i" -> Int_i
            | "o" -> Int_o
            | "x" -> Int_x
            | "X" -> Int_X
            | "u" -> Int_u
            | "d" | _ -> Int_d
          end
      end
    | Some m ->
      begin match modal.variant with
        | None -> begin match m with
            | Space -> Int_sd
            | Plus -> Int_pd
            | _ -> Int_d
          end
        | Some s ->
          match m, s with
          | Space, "i" -> Int_si
          | Plus, "i" -> Int_pi
          | Space, "d" -> Int_sd
          | Plus, "d" -> Int_pd
          | Hash, "x" -> Int_Cx
          | Hash, "X" -> Int_CX
          | Hash, "o" -> Int_Co
          | _ -> Int_d
      end
  in
  Format.fprintf ppf I.(Format(f variant padding precision,"")) n


let print_float (params: _ list) ppf modal n =
  let open Modal in
  let padding, params =
    match modal.padding, params with
    | None, _ -> I.No_padding, params
    | Some (Lit k), _ -> I.Lit_padding (Right,k), params
    | Some Star, k :: q -> I.Lit_padding (Right,k), q
    | Some Star, [] -> assert false  in
  let precision = match modal.precision, params with
    | None, _ -> I.No_precision
    | Some(Lit k), _ -> I.Lit_precision k
    | Some Star, k :: _ -> I.Lit_precision k
    | Some Star, [] -> assert false in
  let variant =
    let open I in
    match modal.modifier with
    | None -> begin
        match modal.variant with
        | None -> Float_f
        | Some x -> begin
            match x with
            | "e" -> Float_e
            | "f" -> Float_f
            | "g" -> Float_g
            | "h" -> Float_h
            | "E" -> Float_E
            | "F" -> Float_F
            | "G" -> Float_G
            | "H" -> Float_H
            | _ -> Float_f
          end
      end
    | Some m ->
      begin match modal.variant with
        | None -> begin match m with
            | Space -> Float_sf
            | Plus -> Float_pf
            | _ -> Float_f
          end
        | Some s ->
          match m, s with
          | Space, "e" -> Float_se
          | Space, "f" -> Float_sf
          | Space, "g" -> Float_sg
          | Space, "h" -> Float_sh
          | Space, "E" -> Float_sE
          | Space, "G" -> Float_sG
          | Space, "H" -> Float_sH
          | Plus, "e" -> Float_pe
          | Plus, "f" -> Float_pf
          | Plus, "g" -> Float_pg
          | Plus, "h" -> Float_ph
          | Plus, "E" -> Float_pE
          | Plus, "G" -> Float_pG
          | Plus, "H" -> Float_pH
          | _ -> Float_f
      end
  in
  Format.fprintf ppf I.(Format(Float(variant,padding,precision,End_of_format),"")) n

let int v pa pr = I.(Int(v,pa,pr,End_of_format))
let int32 v pa pr = I.(Int32(v,pa,pr,End_of_format))
let int64 v pa pr = I.(Int64(v,pa,pr,End_of_format))
let nativeint v pa pr = I.(Nativeint(v,pa,pr,End_of_format))



let print_string (params:_ list) ppf modal n =
  let open Modal in
  let padding =
    match modal.padding, params with
    | None, _ -> I.No_padding
    | Some (Lit k), _ -> I.Lit_padding (Right,k)
    | Some Star, k :: _ -> I.Lit_padding (Right,k)
    | Some Star, [] -> assert false  in
  let variant =
    let open I in
    match modal.variant with
    | Some "S" -> Caml_string(padding,End_of_format)
    | None | Some "s" | Some _ -> String(padding,End_of_format) in
  Format.fprintf ppf I.(Format(variant,"")) n



let print_bool (params:int list) ppf modal n =
  let open Modal in
  let padding =
    match modal.padding, params with
    | None, _ -> I.No_padding
    | Some (Lit k), _ -> I.Lit_padding (Right,k)
    | Some Star, k :: _ -> I.Lit_padding (Right,k)
    | Some Star, [] -> assert false in
  let variant =
    let open I in
    match modal.variant with
    | Some "B" -> Bool(padding,End_of_format)
    | None | Some "b" | Some _ -> Bool(padding,End_of_format) in
  Format.fprintf ppf I.(Format(variant,"")) n

let print_char ppf modal n =
  let open Modal in
  let variant =
    let open I in
    match modal.variant with
    | Some "C" -> Caml_char(End_of_format)
    | None | Some "c" | Some _ -> Char(End_of_format) in
  Format.fprintf ppf I.(Format(variant,"")) n


let print_elt  (type x) ?(params=([]:_ list))
    ppf modal (w: x W.s) (x:x) = match w with
  | W.Int -> print_int params int ppf modal x
  | W.Char -> print_char ppf modal x
  | W.Bool -> print_bool params ppf modal x
  | W.Int32 -> print_int params int32 ppf modal x
  | W.Int64 -> print_int params int64 ppf modal x
  | W.Nativeint -> print_int params nativeint ppf modal x
  | W.Float -> print_float params ppf modal x
  | W.String -> print_string params ppf modal x

let print_hole (type x l fl) ppf modal
    (w:<x:x; l:l; fl:fl; driver:Format.formatter; mid:unit > W.arg) (x:x) =
  match w with
  | W.A -> let W.Show(f,x) = x in (f ppf x:unit)
  | W.T -> (x ppf:unit)
  | W.(S w) -> print_elt ppf modal w x
  | W.(Int_param w) -> print_elt ~params:[fst x] ppf modal w (snd x)
  | W.(Int2_param w) ->
    let pa,pr,x = x in
    print_elt ~params:[pa;pr] ppf modal w x

let rec kfprintf: type l r. (formatter -> r) ->
  formatter -> (l,formatter,unit) t -> (l,r) L.t
  -> r  =
  fun k ppf x args ->
    match x with
    | [] -> k ppf
    | Text s :: q ->
      pp_print_string ppf s; kfprintf k ppf q args
    | Hole{modal; arg; pos} :: q ->
      print_hole ppf modal arg (nth pos args); kfprintf k ppf q args
    | Open_box {kind;indent} :: q ->
      Format.fprintf ppf "@[<%s%d>" (Formatting_box.to_string kind) indent;
      kfprintf k ppf q args
    | Open_tag s :: q -> Format.pp_open_tag ppf s; kfprintf k ppf q args
    | Close_box :: q ->
      Format.pp_close_box ppf (); kfprintf k ppf q args
    | Close_tag :: q -> Format.pp_close_tag ppf (); kfprintf k ppf q args
    | Break {space;indent} :: q -> Format.pp_print_break ppf space indent;
      kfprintf k ppf q args
    | Fullstop :: q -> Format.fprintf ppf "@."; kfprintf k ppf q args
    | Newline :: q -> Format.pp_force_newline ppf (); kfprintf k ppf q args
    | Flush :: q -> Format.fprintf ppf "%!"; kfprintf k ppf q args

let rec expand_full: type l m r r f mid. (l * m, r * r, f, mid ) W.l
  -> ((l,r) L.t -> r) -> m =
  fun spec f -> match spec with
    | W.(S x :: q)  ->
      fun n -> expand_full q (fun l -> f L.(n :: l) )
    | W.(A :: q) ->
      fun show x -> expand_full q (fun l -> f L.(W.Show(show,x) :: l))
    | W.(T :: q) ->
      fun t -> expand_full q (fun l -> f L.(t :: l))
    | W.(Int_param x :: q) ->
      fun n x -> expand_full q (fun l -> f L.((n,x) :: l) )
    | W.(Int2_param x :: q) ->
      fun n m x -> expand_full q (fun l -> f L.((n,m,x) :: l) )
    | W.[] -> f []

let rec expand: type f l m r mid. (l * m, r * r, f, mid ) W.l
  -> ((l,r) L.t -> r) -> l =
  fun spec f -> match spec with
    | W.(S _ :: q)  ->
      fun n -> expand q (fun l -> f L.(n :: l) )
    | W.(A :: q) ->
      fun x -> expand q (fun l -> f L.(x :: l))
    | W.(T :: q)  ->
      fun t -> expand q (fun l -> f L.(t :: l) )
    | W.(Int_param _ :: q)  ->
      fun t -> expand q (fun l -> f L.(t :: l) )
    | W.(Int2_param _ :: q)  ->
      fun t -> expand q (fun l -> f L.(t :: l) )
    | W.[] -> f []


let int pos = Hole{ modal=Modal.default; arg=S Int; pos }
let float pos =  Hole{ modal=Modal.default; arg=S Float; pos }
let str pos = Hole{ modal=Modal.default; arg=S String; pos }
let show pos =  Hole{ modal=Modal.default; arg=A; pos }

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

  let kfprintf (type x y r)
      (k:Format.formatter -> r)
      (W.H spec:(x, r, r, Format.formatter,unit) W.h)
      { u = Box(spec',metafmt) }:
    Format.formatter -> x =
    match W.leq spec spec' with
    | Some W.Eq -> fun ppf -> expand_full spec @@ kfprintf k ppf metafmt
    | None -> raise Metafmt_type_error


  let fprintf spec = kfprintf (fun _ -> ()) spec

  let sprintf spec u =
    let b = Buffer.create 10 in
    kfprintf (fun _ -> Buffer.contents b) spec u (Format.formatter_of_buffer b)

end
