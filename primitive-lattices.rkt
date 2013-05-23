#lang racket

(require "data.rkt")
(provide make-flat-semi-lattice
         flat-equal?-semi-lattice
         flat-eqv?-semi-lattice
         flat-eq?-semi-lattice
         make-flat-lattice
         flat-equal?-lattice
         flat-eqv?-lattice
         flat-eq?-lattice
         ascending-semi-lattice-on-numbers
         descending-semi-lattice-on-numbers)

(module+ test (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flat Semi-Lattices

(define (make-flat-semi-lattice equivalence-predicate equivalence-hash-code)
  (semi-lattice (lambda (x y)
                  (if (equivalence-predicate x y)
                      x
                      (error 'join
                             "no join of ~a and ~a in an unbounded flat semi-lattice"
                             x y)))
                equivalence-predicate
                equivalence-predicate
                equivalence-hash-code))

(define flat-equal?-semi-lattice
  (make-flat-semi-lattice equal? (lambda (x _) (equal-hash-code x))))

(define flat-eqv?-semi-lattice
  (make-flat-semi-lattice eqv? (lambda (x _) (eqv-hash-code x))))

(define flat-eq?-semi-lattice
  (make-flat-semi-lattice eq? (lambda (x _) (eq-hash-code x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flat Lattices

(define (make-flat-lattice equivalence-predicate equivalence-hash-code)
  (lattice (lambda (x y)
             (if (equivalence-predicate x y)
                 x
                 (error 'join
                        "no join of ~a and ~a in an unbounded flat lattice"
                        x y)))
           equivalence-predicate
           (lambda (x y)
             (if (equivalence-predicate x y)
                 x
                 (error 'meet
                        "no meet of ~a and ~a in an unbounded flat lattice")))
           equivalence-predicate
           equivalence-predicate
           equivalence-hash-code))

(define flat-equal?-lattice
  (make-flat-lattice equal? (lambda (x _) (equal-hash-code x))))

(define flat-eqv?-lattice
  (make-flat-lattice eqv? (lambda (x _) (eqv-hash-code x))))

(define flat-eq?-lattice
  (make-flat-lattice eq? (lambda (x _) (eq-hash-code x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Number Semi-Lattices

(define ascending-semi-lattice-on-numbers
  (semi-lattice max >=
                (lambda (x y) (and (number? x) (number? y)))
                equal-hash-code))
(module+ test
  (let ((join (semi-lattice-join ascending-semi-lattice-on-numbers))
        (gte? (semi-lattice-gte? ascending-semi-lattice-on-numbers)))
    (check-equal? (join 0 1) 1)
    (check-equal? (join -1 1) 1)
    (check-equal? (join 10 100) 100)
    (check-false (gte? 0 1))
    (check-true (gte? 0 0))
    (check-true (gte? 0 -1))
    (check-true (gte? 100 10))))

(define descending-semi-lattice-on-numbers
  (semi-lattice min <=
                (lambda (x y) (and (number? x) (number? y)))
                equal-hash-code))
(module+ test
  (let ((join (semi-lattice-join descending-semi-lattice-on-numbers))
        (gte? (semi-lattice-gte? descending-semi-lattice-on-numbers)))
    (check-equal? (join 0 1) 0)
    (check-equal? (join -1 1) -1)
    (check-equal? (join 10 100) 10)
    (check-true (gte? 0 1))
    (check-true (gte? 0 0))
    (check-false (gte? 0 -1))
    (check-false (gte? 100 10))))
