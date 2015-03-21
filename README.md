FatTire: Fault Tolerating Regular Expressions
================================

FatTire is a language for specifying fault-tolerant network forwarding policies. For more details, see the paper: <http://www.cs.cornell.edu/~reitblatt/papers/fault-tolerance-hotsdn13.pdf>

Building from Source
====================

Prerequisites
-------------

- OCaml 4 or higher <http://caml.inria.fr/download.en.html>

- OPAM <http://opam.ocaml.org>

- The following OCaml libraries:

  - findlib
  - core
  - async
  - cstruct
  - oUnit
  - frenetic

  These are available on OPAM:

  ```
  $ opam install ocamlfind core async cstruct ounit frenetic
  ```

Building
--------

- From the ocaml/ directory of the repository, run `make`

  ```
  $ make
  ```
