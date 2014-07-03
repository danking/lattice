#lang racket

(require "data.rkt"
         "primitive-lattices.rkt"
         "operations.rkt")
(provide make-bounded-flat-semi-lattice
         bounded-flat-equal?-semi-lattice
         bounded-flat-eqv?-semi-lattice
         bounded-flat-eq?-semi-lattice
         make-bounded-flat-lattice
         bounded-flat-equal?-lattice
         bounded-flat-eqv?-lattice
         bounded-flat-eq?-lattice
         lattice-on-truth
         lattice-on-numbers
         bounded-lattice-on-numbers)

(module+ test (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bounded Flat Semi-Lattices

(define (make-bounded-flat-semi-lattice equivalence-predicate equivalence-hash-code)
  (make-bounded-semi-lattice (make-flat-semi-lattice equivalence-predicate
                                                     equivalence-hash-code)))

(define-values
  (bounded-flat-equal?-semi-lattice bfsl-equal-identity bfsl-equal-identity?)
  (make-bounded-semi-lattice flat-equal?-semi-lattice))
(define-values
  (bounded-flat-eqv?-semi-lattice bfsl-eqv-identity bfsl-eqv-identity?)
  (make-bounded-semi-lattice flat-eqv?-semi-lattice))
(define-values
  (bounded-flat-eq?-semi-lattice bfsl-eq-identity bfsl-eq-identity?)
  (make-bounded-semi-lattice flat-eq?-semi-lattice))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bounded Flat Lattices

(define (make-bounded-flat-lattice equivalence-predicate equivalence-hash-code)
  (make-bounded-lattice (make-flat-lattice equivalence-predicate
                                           equivalence-hash-code)))

(define-values
  (bounded-flat-equal?-lattice
   bfl-equal-top
   bfl-equal-top?
   bfl-equal-bottom
   bfl-equal-bottom?)
  (make-bounded-lattice flat-equal?-lattice))
(define-values
  (bounded-flat-eqv?-lattice
   bfl-eqv-top
   bfl-eqv-top?
   bfl-eqv-bottom
   bfl-eqv-bottom?)
  (make-bounded-lattice flat-eqv?-lattice))
(define-values
  (bounded-flat-eq?-lattice
   bfl-eq-top
   bfl-eq-top?
   bfl-eq-bottom
   bfl-eq-bottom?)
  (make-bounded-lattice flat-eq?-lattice))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bounded Number Semi-Lattices

(define-values
  (bounded-ascending-semi-lattice-on-numbers baslon-bottom baslon-bottom?)
  (make-bounded-semi-lattice ascending-semi-lattice-on-numbers))
(module+ test
  (let ((join (semi-lattice-join bounded-ascending-semi-lattice-on-numbers))
        (gte? (semi-lattice-gte? bounded-ascending-semi-lattice-on-numbers)))
    (check-true (baslon-bottom? baslon-bottom))
    (check-equal? (join 0 baslon-bottom) 0)
    (check-equal? (join 10 baslon-bottom) 10)
    (check-equal? (join -10 baslon-bottom) -10)
    (check-true (gte? 0 baslon-bottom))
    (check-true (gte? 10 baslon-bottom))
    (check-true (gte? -10 baslon-bottom))
    (check-false (gte? baslon-bottom 0))
    (check-false (gte? baslon-bottom 10))
    (check-false (gte? baslon-bottom -10))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Truth Lattices

(define lattice-on-truth
  (construct-lattice-from-semi-lattices truth-top-boolean-semi-lattice
                                        truth-bottom-boolean-semi-lattice))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Number Lattices

(define lattice-on-numbers
  (construct-lattice-from-semi-lattices ascending-semi-lattice-on-numbers
                                        descending-semi-lattice-on-numbers))

(define bounded-lattice-on-numbers
  (construct-bounded-lattice-from-semi-lattices
   bounded-ascending-semi-lattice-on-numbers
   bounded-descending-semi-lattice-on-numbers))
