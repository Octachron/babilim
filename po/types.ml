type multiline = string list
type comments =
  {
    translator: multiline;
    programmer: multiline
  }

type loc = { file:string; line:int}

type msg =
  | Singular of { id:multiline; translation:multiline }
  | Plural of {id:multiline;plural:multiline; translations: (int * multiline) list }

type entry = {
  comments: comments;
  location: loc;
  flags: string list;
  context: multiline;
  previous: msg option;
  msg: msg;
}

let id = function
  | Singular {id;_} -> id
  | Plural {id;_} -> id

type key = { id:multiline; ctx:multiline }


module Header = struct
  module Map = Map.Make(struct
      type t = string
      let compare=compare
    end)

  type entry = { extra: string list; keyed: string Map.t }
  type t = entry Map.t

  let parse_entry map s =
    if s = "" then map else
    let s= String.trim (Scanf.unescaped s) in
    let p = try String.index s ':' with Not_found ->
      String.length s in
    let section = String.sub s 0 p in
    let entry =
      let start = { extra = []; keyed = Map.empty } in
      String.sub s (p+1) (String.length s - p - 1)
      |> String.split_on_char ';'
      |> List.fold_left (fun entry x ->
          match String.split_on_char '=' x with
          | [] -> assert false
          | [""] -> entry
          | [x] -> let x = String.trim x in
            { entry with extra = x :: entry.extra }
          | k :: x ->
            let k, x = String.(trim k, concat "=" x) in
            { entry with keyed = Map.add k x entry.keyed }
        ) start in
      Map.add section entry map

  let parse s = List.fold_left parse_entry Map.empty s


end

module Map = Map.Make(struct
    type t =  key
    let compare (x:t) (y:t) = compare x y
  end
  )



type map = entry Map.t

type po = { header: Header.t; lang:string; plural:Metaprintf.Math_expr.any; map:map }

let add entry map = Map.add { id= id entry.msg; ctx= entry.context } entry map

let of_list = List.fold_left (fun m x -> add x m) Map.empty

let make msg map =
  let contents = match msg with
    | Singular x -> x.translation
    | Plural {translations = (_, a) :: _; _  } -> a
    | Plural _ -> failwith "Ill-formed header" in
  let header = try Header.parse contents
    with e -> Format.eprintf "Header parsing failure@."; raise e
  in
  let open Header in
  let lang =
    try
      match Map.find "Language" header with
      | { extra = a :: _; _ } -> a
      | _ -> assert false
    with exn -> Format.eprintf "Language not found @."; exit 2
  in
  let p = try Map.find "Plural-Forms" header
    with Not_found ->  Format.eprintf "Not found: Plural-Forms @. "; exit 2
  in
  let e = try Map.find "plural" p.keyed with Not_found ->
    Format.eprintf "Not found: Plural-Forms.plural@."; exit 2 in

  let plural =
    try
      Math.expr Math_lex.main (Lexing.from_string e)
    with exn ->
      Format.eprintf "Math parser failure while parsing [%s]@." e;
      raise exn
  in
  { header; lang; plural; map }


module Pp= struct

let text ppf s = Format.fprintf ppf "\"%s\"" (Escape.string s)


let list = Format.pp_print_list

let fp = Format.fprintf
let premtext ?(pre="") ppf =
  list ~pp_sep:(fun ppf () -> fp ppf "@,") (fun ppf -> fp ppf "%s%a" pre text)
    ppf

let mtext = premtext ~pre:""

let msg ?(pre="") ppf = function
  | Singular {id;translation} ->
    fp ppf "%smsgid %a@," pre mtext id;
    if translation = [] then ()
    else fp ppf "%smsgstr %a@," pre mtext translation;
  | Plural { id; plural; translations } ->
    fp ppf "%smsgid %a@," pre mtext id ;
    fp ppf "%smsgid_plural %a@," pre mtext plural;
    let pr (n,l) = match l with
    | [] -> ()
    | a :: q ->
      fp ppf "%smsgstr[%d] %a@," pre n text a;
      premtext ~pre ppf q in
    List.iter pr translations


let entry ppf (entry:entry) =
  let f x = Format.fprintf ppf x in
  f "@[<v>";
  List.iter (f "# %s@,") entry.comments.translator;
  List.iter (f"#. %s@,") entry.comments.programmer;
  List.iter (f "#, %s@,") entry.flags;
  f "#: %s:%d@," entry.location.file entry.location.line;
  List.iter (f "msgctxt %a@," text) entry.context;
  Option.( entry.previous >> msg ~pre:"#| " ppf );
  msg ppf entry.msg;
  f "@]@."


end
