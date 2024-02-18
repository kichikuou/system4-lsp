(lang dune 3.0)
(using menhir 2.1)

(name system4_lsp)
(version 0.0.3)

(generate_opam_files true)

(source
 (github kichikuou/system4-lsp))

(authors "kichikuou <KichikuouChrome@gmail.com>")

(maintainers "kichikuou <KichikuouChrome@gmail.com>")

(license GPL-2.0-or-later)

(documentation https://github.com/kichikuou/system4-lsp)

(package
 (name system4_lsp)
 (synopsis "Language server for System4")
 (description "A language server for AliceSoft's System4 language.")
 (depends
  ocaml
  base
  stdio
  camlzip
  dune
  dune-build-info
  (linol-lwt (>= 0.6))
  menhir
  ppx_yojson_conv_lib
  (ppx_expect :with-test)))
