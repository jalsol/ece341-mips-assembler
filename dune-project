(lang dune 3.14)

(name mips_assembler)

(generate_opam_files true)

(source
 (github jalsol/mips_assembler))

(authors "Quang-Truong Nguyen")

(maintainers "Quang-Truong Nguyen")

(package
 (name mips_assembler)
 (synopsis "MIPS Assembler")
 (description "An MIPS Assembler for the ECE343 lab project.")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
