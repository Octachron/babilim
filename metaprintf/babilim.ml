let dir = Configuration.share ^ "/babilim/"
module T = Tmap

let set_map f =
  match T.Store.read @@ dir ^ f ^ ".bo" with
  | None -> ()
  | Some m ->
    let map = m.T.Store.translations in
    let T.Implementation.{kfprintf; knfprintf } = T.Implementation.from_map map in
      I18n.hook := { I18n.kfprintf; I18n.knfprintf }

let () =
  Clflags.add_arguments __LOC__
    [
      "-x-lang",
      Arg.String set_map,
      " <name>: translate warning and erors message to <name>"
    ]
