(lang dune 3.21)

(using ctypes 0.3)

(name janet-ml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name janet-ml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml ctypes ppx_jane core)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
