let dir = ""
module T = Metaprintf.Tmap
module I18n = struct
  let implementation = ref T.Implementation.default
end

let set_map f =
  match T.Store.read @@ f ^ ".bo" with
  | None -> ()
  | Some m ->
    let map = m.T.Store.translations in
    I18n.implementation := T.Implementation.from_map map

let () =
  Clflags.add_arguments __LOC__
    [
      "-x-lang",
      Arg.String set_map,
      " <name>: translate warning and erors message to <name>"
    ]
