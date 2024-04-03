(lang dune 3.14)

(name jalmas)

(generate_opam_files true)

(source
 (github jalsol/jalmas))

(authors "Quang-Truong Nguyen")

(maintainers "Quang-Truong Nguyen")

(package
 (name jalmas)
 (synopsis "jalmas - just a little mips assembler")
 (description "An MIPS Assembler for the ECE343 lab project.")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
