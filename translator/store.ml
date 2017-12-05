
let magic = "Babilim.Translation.Store"

module Mf = Metaprintf

module M = Map.Make(struct type t = string let compare = compare end)
type map = { lang:string; fmt: Mf.Tmap.t }

let write (map:map) s =
  let ch = open_out_bin s in
  output_string ch magic;
  output_value ch map

let read s =
  let ch = open_in_bin s in
  if really_input_string ch (String.length magic) = magic then
    let map: map = input_value ch in
    Some map
  else
    None
