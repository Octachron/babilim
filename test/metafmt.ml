open Metaprintf
module C = Metaprintf.Conv

module M = Metaprintf.Metafmt
module W = Metaprintf.Witness

module Metafmt = struct
  open M

  let f ppf =
    print ppf
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

let m = C.Map.empty

let m = C.Map.add "%d/%d" W.[S Int; S Int]
    (fun ppf -> Format.fprintf ppf "A nice ratio %d/%d") m

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

let m = C.meta_add "%d/%s %a" W.[S Int; S String; A]
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

let () =
  Format.printf "Translation map test:\n";
  C.Map.find "%d/%s %a" m Format.std_formatter 2 "times"
    Format.pp_print_string "to Ω";
  Format.printf "@.";

module Dyn = struct

  let spec: _ format6 = "%d %s %a"
  let x =
    "Behold α:%2$a\n\
     A text with a variable %0$d that appears %0$d %1$s"

  let y = Parser.metafmt Lexer.main (Lexing.from_string x)
      spec

  let add (Untyped.Dyn { spec; ref; fmt } ) m =
    C.meta_add ref spec fmt m

  let m = add y m
  let () =
    Format.printf "Dynamic metafmt:\n";
    C.Map.find spec m Format.std_formatter 2 "times"
      Format.pp_print_string "to β";
    Format.printf "@."

end

module Generator = struct
 let gen =
    Format.printf "Generated code:\n%a\n"
      Untyped.gen Dyn.y
 end
