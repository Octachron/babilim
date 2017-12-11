open Metaprintf
module C = Metaprintf.Conv

module M = Metaprintf.Metafmt
module W = Metaprintf.Witness

module Metafmt = struct
  open M

  let f ppf =
    kfprintf (fun _ -> ()) ppf
      [
        Text "Behold α";
        show _2;
        Text "A text with a variable";
        int _0;
        Text "that appears";
        int _0;
        str _1
      ]

  let g = expand W.[S Int;S String; A] (f Format.std_formatter)

let () =
  indexed_apply (Format.printf "Indexed apply: [%s %s %s %s %d]\n")
    Meta.[Z;Z;Z;Z;S Z] L.[ "Hi"; 1 ]

end

let m = Tmap.empty

module Witness = struct
open W
let box =
  let f ppf i (Show(f,x)) =
    Format.fprintf ppf "Box test [i:%d show:%a]\n"
      i f x
  in
  Box([S Int;A],f)

let () =
  unbox (H [S Int;A]) box Format.std_formatter 6 Format.pp_print_string "Hi"

end

let m = Tmap.box_then_add "%d/%s %a" W.[S Int; S String; A]
      M.[
        Text "Behold α:";
        show _2;
        Text "\nA text with a variable ";
        int _0;
        Text " that appears ";
        int _0;
        Text" ";
        str _1;
    ] m

let xprintf x = Tmap.xfprintf m Format.std_formatter x
let () =
  Format.printf "Translation map test:\n";
  xprintf "%d/%s %a"
    2
    "times"
    Format.pp_print_string "to Ω";
  Format.printf "@.";

module Dyn = struct

  let spec: _ format6 = "%d %s %a"
  let x =
    "@[<v 2>@,\
     Behold @{<greek>α@}:%3$a@;\
     @[A text with a variable %1$d that appears %1$d %2$s@]@;\
     @]"


  let parse spec x = Parser.metafmt Lexer.main (Lexing.from_string x)
      spec

  let y = parse spec x

  let spec': _ format6 = "%d %f %s"
  let r = "A seemingly classic format, %d = %f, %s"
  let s = parse spec' r

  let add = Tmap.add
  let m = m |> add s |> add y

  let xprintf x = Tmap.xfprintf m Format.std_formatter x
  let () =
    Format.printf "@.@[<v>Dynamic metafmt:";
    xprintf spec
      2
      "times"
      Format.pp_print_string "to β";
    Format.printf "@,@[<v 2>Dynamic metafmt 2:@,";
    xprintf spec'
      1 1. "isn'it?";
    Format.printf "@]@]@."

end

module Generator = struct
 let gen =
    Format.printf "Generated code:\n%a\n"
      Untyped.gen Dyn.y
 end
