let dir = Configuration.share ^ "/babilim/"
module T = Tmap

let set_map f =
  match T.Implementation.from_store (dir ^ f) with
  | None -> ()
  | Some T.Implementation.{kfprintf; knfprintf } ->
      I18n.hook := { I18n.kfprintf; I18n.knfprintf }

let () =
  Clflags.add_arguments __LOC__
    [
      "-x-lang",
      Arg.String set_map,
      " <name>: translate warning and erors message to <name>"
    ]
