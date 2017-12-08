

module T = Metaprintf.Tmap
module I18n = struct
  let implementation = ref T.Implementation.default

  let kfprintf ppf fmt = !implementation.kfprintf ppf fmt

end


let set_map f =
  match T.Store.read @@ f ^ ".bo" with
  | None -> ()
  | Some m ->
    let map = m.T.Store.translations in
    I18n.implementation := T.Implementation.from_map map

let () = Arg.parse ["-lang", Arg.String set_map, "Set the translation map used"]
    ignore "core -lang name"

let () =
  I18n.kfprintf ignore Format.std_formatter "This is the message %d over %d" 1 2;
  Format.printf "@."
