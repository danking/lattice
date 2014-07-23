#lang racket

(require "tri-partition-set.rkt"
         (prefix-in gen: "generics.rkt")
         )
(provide set-add/lattice-join
         set-add/lattice-meet
         )

(define (set-add/lattice cmp s x)
  (define-values (greater-or-eq lesser incomparable)
    (tri-partition-set (lambda (y) (cmp y x))
                       (lambda (y) (cmp x y))
                       s))
  (if (set-empty? greater-or-eq)
      (set-add incomparable x)
      s))

(define set-add/lattice-join (curry set-add/lattice gen:gte?))
(define set-add/lattice-meet (curry set-add/lattice gen:lte?))
