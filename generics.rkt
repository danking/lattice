#lang racket

(require racket/generic)
(provide (except-out (all-defined-out)
                     bounded-join-semi-lattice?
                     bounded-meet-semi-lattice?)
         (rename-out (really-bounded-join-semi-lattice?
                      bounded-join-semi-lattice?)
                     (really-bounded-meet-semi-lattice?
                      bounded-meet-semi-lattice?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Primer on Lattices
;;
;; Sometimes these symbols are used to refer to the operations within
;;
;;    join -- ⊔     \sqcup
;;    meet -- ⊓     \sqcap
;;    gte? -- ⊒     \sqsupseteq
;;    lte? -- ⊑     \sqsubseteq
;;    bottom -- ⊥   \bot
;;    top -- ⊤      \top
;;
;; There are some lattice laws. For a join-semi-lattice,
;;
;;    x join y = z  iff  y join x = z        i.e., join is commutative
;;    x join x = x                           i.e., join is reflexive
;;    x join bottom = x                      i.e., bottom is the identity
;;
;;    (x gte? y) and (y gte? z)  implies  x gte? z  i.e., gte? is transitive
;;    x gte? x
;;    x gte? bottom = true
;;
;;    x gte? y  iff  x join y = x
;;
;; Lattices are a combination of two semi-lattices. A meet-semi-lattice must
;; follow the same laws as a join-semi-lattice (in fact, they're algebraic
;; duals).
;;
;; Additionally, forall x and y,
;;
;;    x gte? y  implies  y lte? x
;;    x join y = x  implies  x meet y = y
;;
;;    x join top = top
;;    x meet bottom = bottom
;;
;;    top gte? x
;;    bottom lte? x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generics join-semi-lattice
  [gte? join-semi-lattice right]
  [join join-semi-lattice right]
  )

(define-generics meet-semi-lattice
  [lte? meet-semi-lattice right]
  [meet meet-semi-lattice right]
  )

(define-generics bounded-join-semi-lattice
  [bottom bounded-join-semi-lattice]
  )

(define-generics bounded-meet-semi-lattice
  [top bounded-meet-semi-lattice]
  )

(define (lattice? x)
  (and (join-semi-lattice? x)
       (meet-semi-lattice? x)))

(define (bounded-lattice? x)
  (and (really-bounded-join-semi-lattice? x)
       (really-bounded-meet-semi-lattice? x)))

(define (really-bounded-join-semi-lattice? x)
  (and (join-semi-lattice? x)
       (bounded-join-semi-lattice? x)))

(define (really-bounded-meet-semi-lattice? x)
  (and (meet-semi-lattice? x)
       (bounded-meet-semi-lattice? x)))