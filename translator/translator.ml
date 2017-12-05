
let input = ref None
let output = ref None

let optref r = Arg.String (fun s -> r := Some s)
let args =
  ["-input", optref input, "input po(t) files";
   "-output", optref output, "output binary map"
  ]


module Mf = Metaprintf

let flatten = String.concat ""
let add _ (entry:Po.Types.entry) map =
  let ctx = flatten entry.context in
  match entry.msg with
  | Singular { id; translation } ->
    let Mf.Untyped.Cfmt.Dyn fmt = Mf.Parse.fmt (flatten id) in
    let to_format = CamlinternalFormatBasics.Format(fmt, flatten id) in
    let metafmt = Mf.Parse.metafmt (flatten translation) to_format in
    Mf.Tmap.add ~ctx metafmt map
  | Plural { id; plural; translations } ->
    let Mf.Untyped.Cfmt.Dyn fmt = Mf.Parse.fmt (flatten plural) in
    let ctx = flatten (ctx::id) in
    let to_format = CamlinternalFormatBasics.Format(fmt, flatten plural) in
    let add m (num,translation) =
      let metafmt = Mf.Parse.metafmt (flatten translation) to_format in
      Mf.Tmap.add ~num ~ctx metafmt m in
    List.fold_left add map translations



let transform file =
  let data = Po.Parse.file file in
  let map = Po.Types.Map.fold add data Mf.Tmap.empty in
  let output = match !output with
    | None -> Filename.chop_extension file ^ ".bo"
    | Some x -> x in
  Store.write { lang="en"; fmt = map } output

let () =
  Arg.parse args ignore "translator -input file";
  Po.Option.( !input >> transform )
