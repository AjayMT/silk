(lang dune 2.6)
(using menhir 2.0)
(name silk)

(generate_opam_files true)

(version 1.0.0)
(source (github AjayMT/silk))
(license MIT)
(authors "Ajay Tatachar")
(maintainers "Ajay Tatachar")

(package
 (name silk)
 (synopsis "The Silk Programming Language")
 (depends
  (dune (>= 2.6.0))
  (nice_parser (>= 1.0.0))
  (menhir (>= 20190626))
  (odoc :with-doc)))
