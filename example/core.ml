

module T = Metaprintf.Tmap
module I18n = struct
  let implementation = ref T.Implementation.default

  let fprintf ppf fmt = !implementation.fprintf ppf fmt
  let str s = !implementation.str s
  let sprintf fmt = !implementation.sprintf fmt

  let printf x = fprintf Format.std_formatter x

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
  I18n.printf "This is the message %d over %d" 1 2;
  Format.printf "@."
