# Racket Lattice Library

The file `main.rkt` exports all the relevant bindings for the library. The
library is organized into four separate files:

  - `data.rkt` -- the data definitions for (bounded-)(semi-)lattices.

  - `primitive-lattices.rkt` -- some trivial flat (semi-)lattices and a
    procedure for lifting equivalence predicates to flat (semi-lattices); also a
    couple semi-lattices on truth values and numbers

  - `derived-lattices.rkt` -- lift the primitive lattices to bounded lattices

  - `operations.rkt` -- procedures for pointwise creating lattices, disjoint
    union lattices, and lattices on dictionaries (i.e. maps)

To run the tests:

    raco test *.rkt
