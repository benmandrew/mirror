(lang dune 3.12)

(name mirror)

(generate_opam_files true)

(source
 (github benmandrew/mirror))

(authors "Ben Andrew")

(maintainers "Ben Andrew")

(license MIT)

(package
 (name mirror)
 (synopsis "Tool for rendering geometric patterns")
 (description "Tool for rendering geometric patterns")
 (depends ocaml dune cairo2 yojson ppx_deriving_yojson))
