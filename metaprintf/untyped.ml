
module Internal = CamlinternalFormatBasics

exception Dynamic_type_error of string
open Metafmt
module W = Witness

module Cfmt = struct

  type s = { label:string; pa:int; pr:string; mode:string }
  type ('fmter,'mid,'fin) t =
      Dyn: ('a,'fmter,'mid,'d,'e,'fin) Internal.fmt ->
      ('fmter,'mid,'fin) t

  type u =
    { u: 'fmter 'mid 'fin.  ('fmter,'mid,'fin) t } [@@unboxed]
  let unsafe (d:(_,_,_) t) = { u = Obj.magic d }

  let nil = Dyn Internal.End_of_format
  exception Not_implemented of string
  open Internal
  let scons (_m, s) (Dyn l) = match s with
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
    | "b" | "B" -> Dyn( Bool(No_padding, l))
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

  let fcons f (Dyn l) =
    Dyn (Formatting_lit(f, l))

  let box_cons kind indent (Dyn l) =
    let box = Format.sprintf "<%s %d>"
        (Metafmt.Formatting_box.to_string kind) indent in
    Dyn (Formatting_gen(
        Open_box(
          Format(String_literal(box,End_of_format),
                 "@["^box)),
        l))


  let tag_cons s (Dyn l) =
    let tag = Format.sprintf "<%s>" s in
    Dyn (Formatting_gen(
        Open_tag(
          Format(String_literal(tag,End_of_format),
                 "@{"^tag)),
        l))

end

type ('finl, 'finr, 'fmter, 'mid ) dyn = Dyn: {
    spec:('a * 'm , 'finl * 'finr, 'fmter, 'mid) W.l;
    ref: ('m,'fmter,'mid,'d,'e,'finr) format6;
    fmt:('a,'fmter, 'mid) t}  ->
    ('finl, 'finr, 'fmter, 'mid )
      dyn

type ('driver,'mid) atom = A: ('a,'driver,'mid) Metafmt.atom ->
  ('driver, 'mid) atom [@@unboxed]


type nat = I: ('x,'src) index -> nat [@@unboxed]
type ('d,'mid) arg = W : <x:'x; fl:_; l:_; mid:'mid; driver:'d > W.arg ->
  ('d,'mid) arg [@@unboxed]


let error s = raise (Dynamic_type_error s)

let (%=%) (type a b) (x:a W.s) (y:b W.s): (a,b) W.eq =
  match W.( x === y ) with
  | None -> error "Incompatible element types"
  | Some W.Eq -> W.Eq

let rec compat: type x x2 src dest a b c d e dr fl fr mid.
  <x:x; l:d; fl:e; driver:dr; mid:mid> W.arg -> (x2,src) index ->
  (dest * a , fl * fr, dr, mid) W.l
  -> (x,dest) index
  =
  let open W in
  fun arg index spec -> match arg, index, spec with
    | _ , _, W.[] -> error "Mismatched position and number of arguments"
    | W.S x, Z, W.S y :: _ -> (match x %=% y with Eq -> Z)
   | W.Int_param x, Z, W.Int_param y :: _ -> (match x %=% y with Eq -> Z)
   | W.Int2_param x, Z, W.Int2_param y :: _ -> (match x %=% y with Eq -> Z)
   | W.A, Z, W.A :: _ -> Z
   | W.T, Z, W.T :: _ -> Z
   | _, S n, _ :: q -> S (compat arg n q)

   (* Error messages *)
   | W.A, Z, W.S _ :: _  -> error "Expexted %a got %simple"
   | W.T, Z, W.S _ :: _ -> error "Expexted %t got %simple"
   | W.S _, Z, W.T :: _  -> error "Expexted %simple got %t"
   | W.S _, Z, W.A :: _ -> error "Expexted %simple got %a"
   | W.T, Z, W.A :: _ -> error "Expected %t got %a"
   | W.A, Z, W.T :: _ -> error "Expected %a got %t"

   | W.Int_param _, Z, W.S _ :: _ -> error "Expected int parameter %* got %simple"
   | W.Int_param _, Z, W.A :: _ -> error "Expected int parameter %* got %a"
   | W.Int_param _, Z, W.T :: _ -> error "Expected int parameter %* got %t"
   | W.Int_param _, Z, W.Int2_param _ :: _ ->
     error "Expected int parameter %*, got %*.* int parameter"

   | W.Int2_param _, Z, W.S _ :: _ ->
     error "Expected 2 int parameters %*.* got %simple"
   | W.Int2_param _, Z, W.A :: _ ->
     error "Expected 2 int parameters %*.* got %a"
   | W.Int2_param _, Z, W.T :: _ ->
     error "Expected 2 int parameters %*.* got %t"
   | W.Int2_param _, Z, W.Int_param _ :: _ ->
     error "Expected 2 int parameters %*.*, got %* single int parameter"

   | W.S _, Z, W.Int_param _ :: _  ->
     error "Expected %simple, got int parameter %*"
   | W.A, Z, W.Int_param _ :: _  ->
     error "Expected %a, got int parameter %*"
   | W.T, Z, W.Int_param _ :: _  ->
     error "Expected %t, got int parameter %*"
   | W.S _, Z, W.Int2_param _ :: _  ->
     error "Expected %simple, got 2 int parameters %*.*"
   | W.A, Z, W.Int2_param _ :: _  ->
     error "Expected %simple, got 2 int parameters %*.*"
   | W.T, Z, W.Int2_param _ :: _  ->
     error "Expected %simple, got 2 int parameters %*.*"



let nil (Internal.Format (core,_) as fmt) =
  let W.H spec = Conv.typer core in
  Dyn {spec;ref=fmt; fmt=[]}

let cons (type finl finr d mid) (A atom: (d,mid) atom)
    (Dyn r: (finl,finr,d,mid) dyn) =
  match atom with
  | Text s -> Dyn { r with fmt = Text s :: r.fmt }
  | Open_box b -> Dyn { r with fmt = Open_box b :: r.fmt }
  | Open_tag x -> Dyn { r with fmt = Open_tag x :: r.fmt }
  | Close_box -> Dyn { r with fmt = Close_box :: r.fmt }
  | Close_tag -> Dyn { r with fmt = Close_tag :: r.fmt }
  | Fullstop -> Dyn { r with fmt = Fullstop :: r.fmt }
  | Newline -> Dyn { r with fmt = Newline :: r.fmt }
  | Break b -> Dyn { r with fmt = Break b :: r.fmt }
  | Flush -> Dyn { r with fmt = Flush :: r.fmt }


  | Hole{modal; arg=(W.S x as arg); pos } ->
    let pos =  compat arg pos r.spec in
    Dyn {r with fmt = Hole{ modal; arg=S x; pos} :: r.fmt }

  | Hole{modal; arg=(W.Int_param x as arg); pos } ->
    let pos =  compat arg pos r.spec in
    Dyn {r with fmt = Hole{ modal; arg=W.Int_param x; pos} :: r.fmt }

  | Hole{modal; arg=(W.Int2_param x as arg); pos } ->
    let pos =  compat arg pos r.spec in
    Dyn {r with fmt = Hole{ modal; arg=W.Int2_param x; pos} :: r.fmt }

  | Hole{modal; arg= (W.A as arg); pos } ->
    let pos =  compat arg pos r.spec in
    Dyn {r with fmt = Hole{modal;arg=W.A; pos } :: r.fmt }
  | Hole{modal; arg= (W.T as arg); pos } ->
    let pos =  compat arg pos r.spec in
    Dyn {r with fmt = Hole{modal; arg = W.T; pos} :: r.fmt }

let hcons (type d m) (modal,(W arg:(d,m) arg), I n) (Dyn r: ('finl,'finr,d,m) dyn) =
  match arg with
  | W.S x ->
    let pos =  compat arg n r.spec in
    Dyn {r with fmt = Hole{modal;arg=S x; pos} :: r.fmt }

  | W.Int_param x ->
    let pos =  compat arg n r.spec in
    Dyn {r with fmt = Hole{modal;arg=Int_param x; pos} :: r.fmt }

  | W.Int2_param x ->
    let pos =  compat arg n r.spec in
    Dyn {r with fmt = Hole{modal;arg=Int2_param x; pos} :: r.fmt }

  | W.A ->
    let pos =  compat arg n r.spec in
    Dyn {r with fmt = Hole{modal;arg=W.A; pos } :: r.fmt }
  | W.T ->
    let pos =  compat arg n r.spec in
    Dyn {r with fmt = Hole{modal; arg = W.T; pos } :: r.fmt }


let rec integer n =
  if n <= 1 then I Z else
    let I n' = integer(n-1) in I (S n')


exception Unsupported of string
let unsupported s= raise (Unsupported s)


let is_star x = x = Some Modal.Star
let promote (type x) (modal:Modal.t)  (x: x W.s) =
  match is_star modal.padding, is_star modal.precision with
  | true,true -> W(Int2_param x)
  | false, true | true, false -> W(Int_param x)
  | false, false -> W(S x)

let arg modal =
  let promote x = promote modal x in
  function
  | "d" | "i" |"x" | "o" |"X" | "O" -> promote Int
  | "f" | "e" | "g" -> promote Float
  | "b" | "B" -> promote Bool
  | "s" | "S" -> promote String
  | "ld" | "li" | "lx" | "lo" | "lX" | "lO" -> promote Int32
  | "Ld" | "Li" | "Lx" | "Lo" | "LX" | "LO" -> promote Int64
  | "nd" | "ni" | "nx" | "no" | "nX" | "nO" -> promote Nativeint
  | "c" | "C" -> W (S Char)
  | "a" -> W A
  | "t" -> W T
  | s -> unsupported s


module Gen = struct


  let ff = Format.fprintf

  let rec gen_pos ppf = function
    | I Z -> ff ppf "Z"
    | I S n -> ff ppf "(S %a)" gen_pos (I n)

  let gen_s ppf (type x) (x:x W.s) = match x with
    | W.Int -> ff ppf "Int"
    | W.Float -> ff ppf "Float"
    | W.Int32 -> ff ppf "Int32"
    | W.Int64 -> ff ppf "Int64"
    | W.Nativeint -> ff ppf "Nativeint"
    | W.Char -> ff ppf "Char"
    | W.Bool -> ff ppf "Bool"
    | W.String -> ff ppf "String"


  let rec gen_w (type d m) ppf (x: (d,m) arg) =
    match x with
    | W W.A -> ff ppf "A"
    | W W.T -> ff ppf "T"
    | W W.(S x) -> ff ppf "S %a" gen_s x
    | W W.(Int_param x) -> ff ppf "Int_param %a" gen_s x
    | W W.(Int2_param x) -> ff ppf "Int2_param %a" gen_s x

  let gen_elt ppf = function
    | Text s -> ff ppf "Text %S" s
    | Hole{modal; arg; pos } ->
      ff ppf "Hole(Modal.default, %a,%a)" gen_w (W arg) gen_pos (I pos)
    | Break r -> ff ppf "Break{space=%d; indent=%d}" r.space r.indent
    | Open_box r -> ff ppf "Open_tag{indent=%d; kind=H}" r.indent
    | Close_tag -> ff ppf "Close_tag"
    | Close_box -> ff ppf "Close_box"
    | Open_tag s -> ff ppf "Open_tag %s" s
    | Newline -> ff ppf "Newline"
    | Fullstop -> ff ppf "Fullstop"
    | Flush -> ff ppf "Flush"

  let rec gen_list ppf (Dyn r) =
    match r.fmt with
    | [] -> ()
    | [a] -> gen_elt ppf a
    | a :: (_ :: _ as q) ->
      ff ppf "%a; %a" gen_elt a gen_list (Dyn { r with fmt = q })

  let gen ppf =
    ff ppf "Metaprintf.(Metafmt.[%a])" gen_list

end
