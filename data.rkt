#lang racket

(provide (struct-out semi-lattice)
         (struct-out bounded-semi-lattice)
         bounded-semi-lattice-from-semi-lattice
         (struct-out lattice)
         (struct-out bounded-lattice)
         bounded-lattice-from-lattice)

;; a [Semi-Lattice FV] is a
;;
;;   (semi-lattice [FV FV -> FV]
;;                 [FV FV -> Boolean]
;;                 [FV FV -> Boolean]
;;                 [FV [Optional [Any -> Number]] -> Number])
;;
;;  where join(x,y) is the least upper bound of x and y,
;;        gte(x,y) is x >= y, or does x subsume y,
;;        comparable? = gte(x,y) OR gte(y,x), and
;;        comparable?-hash-code is a hash code consistent with comparable?
;;
;; NB: I termed everything in terms of join semi-lattices. The dual notion of
;;     meet semi-lattices is equally accurate, swapping join and gte for meet
;;     and lte. The types do not change
;;
(struct semi-lattice (join gte? comparable? comparable?-hash-code))

;; a [Bounded-Semi-Lattice FV] is a
;;
;;   (bounded-semi-lattice [FV FV -> FV]
;;                         [FV FV -> Boolean]
;;                         [FV FV -> Boolean]
;;                         [FV [Optional [Any -> Number]] -> Number]
;;                         FV)
;;
;;  where the first four arguments are the same as [Lattice FV], and
;;        identity is the element such that join(x, identity) = x
;;
(struct bounded-semi-lattice semi-lattice (identity))

(define (bounded-semi-lattice-from-semi-lattice unbounded-semi-lattice identity)
  (bounded-semi-lattice (semi-lattice-join unbounded-semi-lattice)
                        (semi-lattice-gte? unbounded-semi-lattice)
                        (semi-lattice-comparable? unbounded-semi-lattice)
                        (semi-lattice-comparable?-hash-code
                         unbounded-semi-lattice)
                        identity))

;; a [Lattice FV] is a
;;
;;   (lattice [FV FV -> FV]
;;            [FV FV -> Boolean]
;;            [FV FV -> FV]
;;            [FV FV -> Boolean]
;;            [FV FV -> Boolean]
;;            [FV [Optional [Any -> Number]] -> Number])
;;
;;  where join(x,y) is the least upper bound of x and y,
;;        gte?(x,y) is x >= y, or does x subsume y,
;;        meet(x,y) is the greatest lower bound of x and y,
;;        lte?(x,y) is x <= y, or does y subsume x,
;;        comparable? = gte(x,y) OR gte(y,x), and
;;        comparable?-hash-code is a hash code consistent with comparable?
;;
(struct lattice (join gte? meet lte? comparable? comparable?-hash-code))

;; a [Bounded-Lattice FV] is a
;;
;;   (lattice [FV FV -> FV]
;;            [FV FV -> Boolean]
;;            [FV FV -> FV]
;;            [FV FV -> Boolean]
;;            [FV FV -> Boolean]
;;            [FV [Optional [Any -> Number]] -> Number]
;;            FV
;;            FV)
;;
;;  where the first six arguments are the same as [Lattice FV], and
;;        top is the element such that meet(x, top) = x
;;          (or, join(x, top) = top)
;;        bottom is the element such that join(x, bottom) = x
;;          (or, meet(x, bottom) = bottom)
;;
(struct bounded-lattice lattice (top bottom))

(define (bounded-lattice-from-lattice unbounded-lattice top bottom)
  (bounded-lattice (lattice-join unbounded-lattice)
                   (lattice-gte? unbounded-lattice)
                   (lattice-meet unbounded-lattice)
                   (lattice-lte? unbounded-lattice)
                   (lattice-comparable? unbounded-lattice)
                   (lattice-comparable?-hash-code unbounded-lattice)
                   top
                   bottom))
