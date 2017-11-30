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

module Map = Map.Make(struct
    type t =  key
    let compare (x:t) (y:t) = compare x y
  end
  )

type map = entry Map.t

let add entry map = Map.add { id= id entry.msg; ctx= entry.context } entry map

let of_list = List.fold_left (fun m x -> add x m) Map.empty

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
  f "#: %s:%d@," entry.location.file entry.location.line;
  List.iter (f "#, %s@,") entry.flags;
  List.iter (f "msgctxt %a@," text) entry.context;
  Option.( entry.previous >> msg ~pre:"#| " ppf );
  msg ppf entry.msg;
  f "@]@."


end
