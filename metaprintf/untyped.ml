
module Internal = CamlinternalFormatBasics

exception Dynamic_type_error
open Metafmt

module Cfmt = struct

  type s = { label:string; pa:int; pr:string; mode:string }
  type ('fmter,'mid,'fin) t =
      Dyn: ('a,'fmter,'mid,'d,'e,'fin) Internal.fmt ->
      ('fmter,'mid,'fin) t

  type u =
    { u: 'fmter 'mid 'fin.  ('fmter,'mid,'fin) t }
  let unsafe (d:(_,_,_) t) = { u = Obj.magic d }

  let nil = Dyn Internal.End_of_format
  exception Not_implemented of string
  open Internal
  let scons s (Dyn l) = match s with
    | "d" -> Dyn( Int(Int_d, No_padding,No_precision,l) )
    | "i" -> Dyn( Int(Int_i, No_padding,No_precision,l) )
    | "x" -> Dyn( Int(Int_x, No_padding,No_precision,l) )
    | "X" -> Dyn( Int(Int_X, No_padding,No_precision,l) )
    | "o" -> Dyn( Int(Int_o, No_padding,No_precision,l) )
    | "O" -> Dyn( Int(Int_o, No_padding,No_precision,l) )
    | "ld" -> Dyn( Int32(Int_d, No_padding,No_precision,l) )
    | "li" -> Dyn( Int32(Int_i, No_padding,No_precision,l) )
    | "lx" -> Dyn( Int32(Int_x, No_padding,No_precision,l) )
    | "lX" -> Dyn( Int32(Int_X, No_padding,No_precision,l) )
    | "lo" -> Dyn( Int32(Int_o, No_padding,No_precision,l) )
    | "lO" -> Dyn( Int32(Int_o, No_padding,No_precision,l) )
    | "Ld" -> Dyn( Int64(Int_d, No_padding,No_precision,l) )
    | "Li" -> Dyn( Int64(Int_i, No_padding,No_precision,l) )
    | "Lx" -> Dyn( Int64(Int_x, No_padding,No_precision,l) )
    | "LX" -> Dyn( Int64(Int_X, No_padding,No_precision,l) )
    | "Lo" -> Dyn( Int64(Int_o, No_padding,No_precision,l) )
    | "LO" -> Dyn( Int64(Int_o, No_padding,No_precision,l) )
    | "nd" -> Dyn( Nativeint(Int_d, No_padding,No_precision,l) )
    | "ni" -> Dyn( Nativeint(Int_i, No_padding,No_precision,l) )
    | "nx" -> Dyn( Nativeint(Int_x, No_padding,No_precision,l) )
    | "nX" -> Dyn( Nativeint(Int_X, No_padding,No_precision,l) )
    | "no" -> Dyn( Nativeint(Int_o, No_padding,No_precision,l) )
    | "nO" -> Dyn( Nativeint(Int_o, No_padding,No_precision,l) )
    | "f" -> Dyn(Float(Float_f, No_padding,No_precision,l) )
    | "g" -> Dyn(Float(Float_g, No_padding,No_precision,l) )
    | "e" -> Dyn(Float(Float_e, No_padding,No_precision,l) )
    | "b" | "B" -> Dyn( Bool l )
    | "c" -> Dyn( Char l )
    | "C" -> Dyn (Caml_char l)
    | "s" -> Dyn(String (No_padding, l))
    | "S" -> Dyn(Caml_string(No_padding,l))
    | "a" -> Dyn(Alpha l)
    | "t" -> Dyn(Theta l)
    | "!" -> Dyn(Flush l)

    | s -> raise (Not_implemented (Format.sprintf "Untyped.Cfmt.scons: %s" s))

  let tcons t (Dyn l) =
    if String.length t = 1 then
      Dyn (Char_literal(t.[0],l))
    else
      Dyn (String_literal(t,l))

end

type ('finl, 'finr, 'fmter, 'mid ) u = Dyn: {
    spec:('a * 'm , 'finl * 'finr, 'fmter, 'mid) W.l;
    ref: ('m,'fmter,'mid,'d,'e,'finr) format6;
    fmt:('a,'fmter, 'mid) t}  ->
    ('finl, 'finr, 'fmter, 'mid )
    u
type ('driver,'mid) atom = A: ('a,'driver,'mid) Metafmt.atom ->
  ('driver, 'mid) atom


type nat = I: ('x,'src) index -> nat [@@unboxed]
type ('d,'mid) arg = W : <x:'x; fl:_; l:_; mid:'mid; driver:'d > W.arg ->
  ('d,'mid) arg [@@unboxed]

let rec compat: type x x2 src dest a b c d e dr fl fr mid.
  <x:x; l:d; fl:e; driver:dr; mid:mid> W.arg -> (x2,src) index ->
  (dest * a , fl * fr, dr, mid) W.l
  -> (x,dest) index option
  =
  let open W in
  fun arg index spec -> match arg, index, spec with
   | _ , _, W.[] -> None
   | W.S x, Z, W.S y :: _ ->
     begin match W.(x === y) with
       | None -> None
       | Some Eq -> Some Z
     end
   | W.A, Z, W.A :: _ -> Some Z
   | _, S n, _ :: q ->
     begin match compat arg n q with
       | Some n -> Some(S n)
       | None -> None
     end
   | _ -> None

let nil (Internal.Format (core,_) as fmt) =
  let W.H spec = Conv.typer core in
  Dyn {spec;ref=fmt; fmt=[]}

let cons (type finl finr d mid) (A atom: (d,mid) atom)
    (Dyn r: (finl,finr,d,mid) u) =
  match atom with
  | Text s -> Dyn { r with fmt = Text s :: r.fmt }
  | Hole(W.S x as arg,n) ->
    begin match compat arg n r.spec with
      | None -> raise Dynamic_type_error
      | Some n -> Dyn {r with fmt = Hole(S x, n ) :: r.fmt }
    end
  | Hole(W.A as arg,n) ->
    begin
      match compat arg n r.spec with
      | None -> raise Dynamic_type_error
      | Some n -> Dyn {r with fmt = Hole(W.A, n ) :: r.fmt }
    end
  | Hole(W.T as arg,n) ->
    match compat arg n r.spec with
    | None -> raise Dynamic_type_error
    | Some n -> Dyn {r with fmt = Hole(W.T, n ) :: r.fmt }

let hcons (type d m) ((W arg:(d,m) arg), I n) (Dyn r: ('finl,'finr,d,m) u) =
  match arg with
  | W.S x ->
    begin match compat arg n r.spec with
      | None -> raise Dynamic_type_error
      | Some n -> Dyn {r with fmt = Hole(S x, n ) :: r.fmt }
    end
  | W.A ->
    begin match compat arg n r.spec with
    | None -> raise Dynamic_type_error
    | Some n -> Dyn {r with fmt = Hole(W.A, n ) :: r.fmt }
    end
  | W.T ->
    begin match compat arg n r.spec with
    | None -> raise Dynamic_type_error
    | Some n -> Dyn {r with fmt = Hole(W.T, n ) :: r.fmt }
    end


let rec integer n =
  if n <= 0 then I Z else
    let I n' = integer(n-1) in I (S n')


exception Unsupported of string
let unsupported s= raise (Unsupported s)

let arg = function
  | "d" | "i" |"x" | "o" |"X" | "O" -> W (S Int)
  | "f" | "e" | "g" -> W (S Float)
  | "b" | "B" -> W (S Bool)
  | "s" | "S" -> W (S String)
  | "ld" | "li" | "lx" | "lo" | "lX" | "lO" ->
    W (S Int32)
  | "Ld" | "Li" | "Lx" | "Lo" | "LX" | "LO" ->
    W (S Int64)
  | "nd" | "ni" | "nx" | "no" | "nX" | "nO" ->
    W (S Nativeint)
  | "c" | "C" -> W (S Char)
  | "a" -> W A
  | "t" -> W T
  | s -> unsupported s

let ff = Format.fprintf


let rec gen_pos ppf = function
  | I Z -> ff ppf "Z"
  | I S n -> ff ppf "(S %a)" gen_pos (I n)

let gen_w (type d m) ppf (x: (d,m) arg) =
  Format.pp_print_string ppf @@
  match x with
  | W W.A -> "A"
  | W W.(S Int) -> "S Int"
  | W W.(S Float) -> "S Float"
  | W W.T -> "S Theta"
  | W W.(S Int32) -> "S Int32"
  | W W.(S Int64) -> "S Int64"
  | W W.(S Nativeint) -> "S Nativeint"
  | W W.(S Char) -> "S Char"
  | W W.(S Bool) -> "S Bool"
  | W W.(S String) -> "S String"

let gen_elt ppf = function
  | Text s -> ff ppf "Text %S" s
  | Hole(x,n) -> ff ppf "Hole(%a,%a)" gen_w (W x) gen_pos (I n)

let rec gen_list ppf (Dyn r) =
  match r.fmt with
  | [] -> ()
  | [a] -> gen_elt ppf a
  | a :: (_ :: _ as q) ->
    ff ppf "%a; %a" gen_elt a gen_list (Dyn { r with fmt = q })

let gen ppf =
  ff ppf "Metaprintf.(Metafmt.[%a])" gen_list

(*
let meta_add (Dyn {fmt;ref;spec}) m =
  *)
