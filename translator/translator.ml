
let input = ref None
let args =
  ["-input", Arg.String (fun s -> input := Some s), "input po(t) files"]

let cycle file =
  let data = Po.Parse.file file in
  Po.Types.Map.iter (fun _ -> Format.printf "%a@." Po.Types.Pp.entry)
    data

let () =
  Arg.parse args ignore "translator -input file";
  Po.Option.( !input >> cycle )
