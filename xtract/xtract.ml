module Iter = Ast_iterator
module L = Longident

let debug fmt = Format.eprintf (fmt ^^ "@.")
open Parsetree

let i18n_key = function
  | { Location.txt = "i18n"; _ }, _ ->  true
  | _ -> false

let status = ref false


type info = { plural: bool; context: string list }
let info_default = { plural = false; context = [] }

let metadata e =
  let label r (l,x) = match (l.Location.txt,x) with
    | L.Lident "plural", _  -> { r with plural = true }
    | L.Lident "context", { pexp_desc = Pexp_constant Pconst_string (s,_) } ->
      { r with context = s :: r.context }
    | _ -> r
  in
  let start = function
    | Some {pexp_desc= Pexp_ident { txt = L.Lident "plural";_}; _ } ->
      { info_default with plural = true }
    | _ -> info_default in
  match e.pexp_desc with
  | Pexp_record (fields,x) -> List.fold_left label (start x) fields
  | _ -> info_default


open Po.Option
module Ty = Po.Types

let make_msg info src =
  if info.plural = true then
    Ty.Plural { id = src; plural = src; translations = [0,src] }
  else
    Ty.Singular { id = src; translation = src }


let make format info loc comment msg =
  {
    Ty.comments = { programmer = comment; translator = [] };
    location =
      (let s = loc.Location.loc_start in
       Lexing.{ file = s.pos_fname; line = s.pos_lnum });
    flags = if format then ["c-format"] else [];
    context = info.context;
    previous = None;
    msg
  }

type 'a return = {return: 'r. 'a -> 'r  } [@@unboxed]
let with_return (type a) f =
  let exception Return of a in
  try f { return = (fun x -> raise (Return x)) } with
  | Return x -> x

let may_return f = with_return (fun r -> f r; None)

let expr_payload filter f attrs =
  let elt {return} attr =
    if filter attr then
      match snd attr with
      | PStr [ { pstr_desc = Pstr_eval (e,_); _}  ] ->
        return (f e)
      | _ -> () in
  may_return (fun return -> List.iter (elt return) attrs)

let const f x = Some(f x)

let attributes attrs =
  expr_payload i18n_key (
    function
    | {pexp_desc= Pexp_ident {txt=L.Lident ("all"|"none" as status);_};_} ->
      Some (status = "all")
    | _ -> None ) attrs


type printf = S of bool * int | P of bool * int | No

let is_printf f = match f.pexp_desc with
  | Pexp_ident { txt=L.(Ldot(Lident "I18n", f)); _ } ->
    begin match f with
      | "printf" | "sprintf" -> S(true,0)
      | "s" -> S (false, 0)
      | "kfprintf" -> S(true,2)
      | "fprintf" -> S(true,1)
      | "fnprintf" -> P(true,2)
      | "snprintf" -> P(true,1)
      | "sn" -> P(false,1)
      | "knprintf" -> P(true,3)
      | _ -> No
    end
  | _ -> No


let exdoc  = function
  | { Location.txt = "ocaml.doc"|"doc"; _ },
    PStr [
      { pstr_desc = Pstr_eval ({
            pexp_desc = Pexp_constant Pconst_string (s,_); _ },_)
      }
    ] -> [s]
  | _ -> []

let exdocs = List.fold_left (fun l x -> exdoc x @ l) []

let strf fail k e = match e.pexp_desc with
  | Pexp_constant Pconst_string (s,_) ->
    k s
  | _ -> fail ()

let str x = strf ignore x


let m = ref Ty.Map.empty

let register entry =
  m := Ty.add entry !m
(*  Format.fprintf ppf "%a" Ty.Pp.entry entry *)

let make_expr format info e =
  make format info_default (e.pexp_loc) (exdocs e.pexp_attributes)

let apply e =
  match e.pexp_desc with
  | Pexp_apply (f, l) ->
    begin match is_printf f with
    | No -> ()
    | S (format, n) -> List.nth_opt l n >>| snd >>
      str (fun s -> register @@ make_expr format info_default e
            @@ make_msg info_default [s])
    | P (format,n) ->
      begin match List.nth_opt l n,List.nth_opt l (n+1) with
        | Some (_,x), Some (_,y) ->
          str (fun id -> str (fun plural ->
              register @@ make_expr format info_default e @@
              Ty.Plural {id = [id]; plural = [plural];
                         translations = [0, [id]; 1, [plural]] }
            ) y
            ) x
        | _ -> ()
      end
    end
  | _ -> ()


let with_attrs attrs k =
    match attributes attrs with
    | Some x ->
      let old = !status in
      fun y -> (status := x; let r = k y in status := old; r )
    | None -> k

let super = Iter.default_iterator

let default x y = match y with
  | None -> x
  | Some y -> y

let expr iter e =
  with_attrs e.pexp_attributes (fun () ->
      apply e;
      let info = default info_default
        @@ expr_payload i18n_key (const metadata) e.pexp_attributes in
      if (List.exists i18n_key e.pexp_attributes || !status) then
        e |> strf (fun () -> super.expr iter e)
          (fun s -> register @@ make_expr true info e @@ make_msg info [s])
      else
        super.expr iter e
    ) ()

let structure_item iter i =
  match i.pstr_desc with
  | Pstr_eval (e,attrs) ->
    with_attrs attrs (iter.Iter.expr iter) e
  | Pstr_attribute a ->
    maybe ((:=) status) (attributes [a])
  | _ -> super.structure_item iter i

let value_binding iter vb =
  with_attrs vb.pvb_attributes (super.value_binding iter) vb

let iter =
  { super with
    expr;
    structure_item;
    value_binding;
  }


let parse file =
  Pparse.parse_implementation Format.err_formatter
    ~tool_name:"ppxtract" file
  |> iter.structure iter

let rec explore prefix filter file0 =
  let file = if prefix = "" then file0
    else if prefix.[String.length prefix - 1] ='/'
         || file0="" || file0.[0]='/' then   prefix ^ file0
    else prefix ^ "/" ^ file0 in
  if String.length file0 > 0 && file0.[0] = '.' && not (file =".") then
    []
  else if Sys.is_directory file then
    List.concat @@ List.map (explore file filter)
      (Array.to_list @@ Sys.readdir file)
  else if filter file then [file] else []

(*
let rec explore ppf prefix filter file =
  let file =
    String.concat "/" @@ List.rev_append prefix [file] in
  if Sys.is_directory file then
    Array.iter (explore ppf (file::prefix) filter) (Sys.readdir file)
  else if filter file then
    parse ppf file
*)

let ml_file f = match Filename.extension f with
  | ".ml" -> true
  | ".mlt" -> true
  | _ -> false


let header =
  Ty.Singular { id = [""]; translation =
                          [ "Content-Type: text/plain; charset=UTF-8\n";
                            "Content-Transfer-Encoding: 8bit\n";
                            "Language: en_US\n";
                            "Plural-Forms: nplurals=2; plural=(n != 1);\n";
                            "Project-Id-Version: OCaml - babilim\n";
                            "POT-Creation-Date: \n";
                            "PO-Revision-Date: \n";
                            "Last-Translator: \n";
                            "Language-Team: \n";
                            "MIME-Version: 1.0\n";
                          ]
           }

let () =
  let f = ref "ocaml.pot" in
  let files = ref [] in
  let register_files file = files := explore "" ml_file file :: !files in
  let output = "-pot", Arg.Set_string f, "output pot file" in
  Arg.parse [output] register_files "ppxtract files";
  let ppf = Format.formatter_of_out_channel (open_out !f) in
  Format.fprintf ppf "@[<v>%a@]@." (Ty.Pp.msg ~pre:"") header;
  List.iter (List.iter parse) !files;
  Ty.Map.iter (fun _ entry -> Format.fprintf ppf "%a@." Ty.Pp.entry entry)
    !m
