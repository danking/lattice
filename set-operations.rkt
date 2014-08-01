#lang racket

(require "tri-partition-set.rkt"
         (prefix-in gen: "generics.rkt")
         )
(provide set-add/lattice-join
         set-add/lattice-meet
         set-add/lattice-join!
         set-add/lattice-meet!
         )

(define (set-add/lattice cmp s item)
  (define already-known? (for/or ([x (in-set s)]) (cmp x item)))
  (cond [already-known? s]
        [else
         (set-add (set-filter s (lambda (x) (not (cmp item x))))
                  item)]))

(define (set-add/lattice! cmp s item)
  (define already-known? (for/or ([x (in-set s)]) (cmp x item)))
  (cond [already-known? #f]
        [else
         (set-filter! s (lambda (x) (not (cmp item x))))
         (set-add! s item)
         #t]))

(define (set-filter s p)
  (for/fold
      ([s* s])
      ([e (in-list (set->list s))])
    (if (p e)
        (set-remove s* e)
        s*)))

(define (set-filter! s p)
  (for ([e (in-list (set->list s))])
    (unless (p e)
      (set-remove! s e))))

(define set-add/lattice-join (curry set-add/lattice gen:gte?))
(define set-add/lattice-meet (curry set-add/lattice gen:lte?))

(define set-add/lattice-join! (curry set-add/lattice! gen:gte?))
(define set-add/lattice-meet! (curry set-add/lattice! gen:lte?))
