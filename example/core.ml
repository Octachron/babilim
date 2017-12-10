

module T = Metaprintf.Tmap
module I18n = struct
  let implementation = ref T.Implementation.default

  let kfprintf k ppf fmt = !implementation.kfprintf k ppf fmt
  let sprintf fmt = let b= Buffer.create 10 in
    let ppf = Format.formatter_of_buffer b in
    kfprintf (fun ppf -> Format.pp_print_flush ppf (); Buffer.contents b) ppf fmt

  let s x = sprintf
      CamlinternalFormat.(Format(String_literal(x,End_of_format),x))

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
  let s = I18n.s
      "This message with \"[@@attribute]\" should not be interpreted as a format" in
  Format.printf "@.%s" s;
  Format.printf "@."
