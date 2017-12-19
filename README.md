Babilim is a prototype for localization plugins for the OCaml compiler.

This plugin currently requires a patch to the OCaml compiler to work.
This patch is available at https://github.com/Octachron/ocaml/tree/i18n .

Once this patched compiler installed, babilim can be installed with
`opam pin add babilim https://github.com/Octachron/babilim.git`.

The localization plugin can then be used with either `ocamlopt`

```
ocamlopt -plugin $(ocamlfind query babilim)/babilim.cmxs -x-lang ${available translation}
```

or `ocamlc`

```
ocamlc -plugin $(ocamlfind query babilim)/babilim.cma -x-lang fr ${available translation}
```


# Available translations:

  * [fr](translations/fr.po)

More translations are more than welcome, see the [template file](translations/ocaml.pot)

# Code structure of babilim


* the xtract executable implements localized string extraction
  using the compiler-libs

* the po library implements the parsing of po files

* the metaprintf library provides a position-aware alternative to format string
whic used the underlying Format formatting engine, dynamic parsers for both
Format format string and the metaprintf variant, and some adaptator between the
two format

* the translator executable translates the po files into the internal binary format used by the babilim plugin
