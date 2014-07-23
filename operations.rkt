#lang racket

(require "data.rkt"
         (for-syntax syntax/parse))
(provide construct-lattice-from-semi-lattices
         construct-bounded-lattice-from-semi-lattices
         get-join-semi-lattice-from-lattice
         get-meet-semi-lattice-from-lattice
         make-bounded-lattice
         make-bounded-lattice/with-top&bottom
         make-bounded-semi-lattice
         lexicographic-pointwise-semi-lattice/2
         pointwise-semi-lattice
         pointwise-bounded-semi-lattice
         pointwise-lattice
         pointwise-bounded-lattice
         lattice-disjoint-union
         make-bounded-dictionary-lattice)

(module+ test
  (require rackunit "primitive-lattices.rkt")
  ;; lattice-on-numbers and lattice-on-truth is defined in derived-lattices.rkt,
  ;; but requiring that file here would cause a circular depdendcy
  (define lattice-on-numbers
    (construct-lattice-from-semi-lattices ascending-semi-lattice-on-numbers
                                          descending-semi-lattice-on-numbers))
  (define lattice-on-truth
    (construct-lattice-from-semi-lattices truth-top-boolean-semi-lattice
                                          truth-bottom-boolean-semi-lattice)))

;; construct-lattice-from-semis : [Semi-Lattice FV]
;;                                [Semi-Lattice FV]
;;                                ->
;;                                [Lattice FV]
;;
;; Given that
;;   join = (semi-lattice-join join-semi-lattice)
;;   meet = (semi-lattice-join meet-semi-lattice)
;; join and meet should obey the absoprtion law:
;;
;;   A join (A meet B) = A meet (A join B) = A
;;
(define (construct-lattice-from-semi-lattices join-semi-lattice meet-semi-lattice)
  (lattice (semi-lattice-join join-semi-lattice)
           (semi-lattice-gte? join-semi-lattice)
           (semi-lattice-join meet-semi-lattice)
           (semi-lattice-gte? meet-semi-lattice)
           (lambda (x y)
             ;; should it be the case that its always comparable in both
             ;; semi-lattices?
             (and ((semi-lattice-comparable? join-semi-lattice) x y)
                  ((semi-lattice-comparable? meet-semi-lattice) x y)))
           (lambda (x [recur equal-hash-code])
             (+ ((semi-lattice-comparable?-hash-code join-semi-lattice) x recur)
                ((semi-lattice-comparable?-hash-code meet-semi-lattice) x recur)))))

;; get-join-semi-lattice-from-lattice : [Lattice FV] -> [Semi-Lattice FV]
(define (get-join-semi-lattice-from-lattice lattice)
  (semi-lattice (lattice-join lattice)
                (lattice-gte? lattice)
                ;; TODO is this correct? if it's comparable in the lattice it
                ;; should be comparable in the X-semi-lattice too, right?
                (lattice-comparable? lattice)
                (lattice-comparable?-hash-code lattice)))

;; get-meet-semi-lattice-from-lattice : [Lattice FV] -> [Semi-Lattice FV]
(define (get-meet-semi-lattice-from-lattice lattice)
  (semi-lattice (lattice-meet lattice)
                (lattice-lte? lattice)
                ;; TODO is this correct? if it's comparable in the lattice it
                ;; should be comparable in the X-semi-lattice too, right?
                (lattice-comparable? lattice)
                (lattice-comparable?-hash-code lattice)))

;; make-bounded-semi-lattice : [Semi-Lattice FV]
;;                             ->
;;                             ∃Identity.
;;                               [Bounded-Semi-Lattice [U FV Top Bottom]]
;;                               Identity
;;                               [Any -> Boolean]
;;
;; The predicate function is a predicate for the Identity type.
;;
(define (make-bounded-semi-lattice semi-lattice)
  ;; prefabs wouldn't work here because prefabs rely on unique names
  (struct identity ())

  (define (bounded-join x y)
    (cond [(identity? x) y]
          [(identity? y) x]
          [else ((semi-lattice-join semi-lattice) x y)]))

  (define (bounded-gte? x y)
    (cond [(identity? y) #t]  ;; everything >= identity
          [(identity? x) #f]  ;; identity >= nothing
          [else ((semi-lattice-gte? semi-lattice) x y)]))

  (define (bounded-comparable? x y)
    (or (identity? x) (identity? y)
        ((semi-lattice-comparable? semi-lattice) x y)))

  (define (bounded-comparable?-hash-code x [recur equal-hash-code])
    (if (or (identity? x))
        (recur x)
        ((semi-lattice-comparable?-hash-code semi-lattice) x recur)))

  (values (bounded-semi-lattice bounded-join
                                bounded-gte?
                                bounded-comparable?
                                bounded-comparable?-hash-code
                                (identity))
          (identity) identity?))

;; construct-bounded-lattice-from-semi-lattices : [Bounded-Semi-Lattice FV]
;;                                                [Bounded-Semi-Lattice FV]
;;                                                ->
;;                                                [Bounded-Lattice FV]
;;
;; The semi-lattices must obey the laws demanded by
;; construct-lattice-from-semi-lattices.
;;
(define (construct-bounded-lattice-from-semi-lattices bounded-join-semi-lattice
                                                      bounded-meet-semi-lattice)
  (bounded-lattice-from-lattice
   (construct-lattice-from-semi-lattices bounded-join-semi-lattice
                                         bounded-meet-semi-lattice)
   (bounded-semi-lattice-identity bounded-meet-semi-lattice)
   (bounded-semi-lattice-identity bounded-join-semi-lattice)))

;; make-bounded-lattice : [Lattice FV]
;;                        ->
;;                        ∃Top∃Bottom.
;;                          [Bounded-Lattice [U FV Top Bottom]]
;;                          Top
;;                          [Any -> Boolean]
;;                          Bottom
;;                          [Any -> Boolean]
;;
;; The last two predicate functions are predicates for the Top and the Bottom
;; types.
;;
(define (make-bounded-lattice lattice)
  ;; prefabs wouldn't work here because prefabs rely on unique names
  (struct top ())
  (struct bottom ())

  (values (make-bounded-lattice/with-top&bottom lattice
                                                (top) top?
                                                (bottom) bottom?)
          (top) top?
          (bottom) bottom?))


;; make-bounded-lattice/with-top&bottom : [Lattice FV]
;;                                        Top
;;                                        [Any -> Boolean]
;;                                        Bottom
;;                                        [Any -> Boolean]
;;                                        ->
;;                                        [Bounded-Lattice [U FV Top Bottom]]
;;
;; The last two predicate functions are predicates for the Top and the Bottom
;; types.
;;
(define (make-bounded-lattice/with-top&bottom lattice top top? bottom bottom?)
  (define (bounded-join x y)
    (cond [(or (top? x) (top? y)) top]
          [(bottom? x) y]
          [(bottom? y) x]
          [else ((lattice-join lattice) x y)]))

  (define (bounded-gte? x y)
    (cond [(or (top? x) (bottom? y)) #t] ;; top >= all, all >= bottom
          [(or (bottom? x) (top? y)) #f] ;; this must be after the prev. case
          [else ((lattice-gte? lattice) x y)]))

  (define (bounded-meet x y)
    (cond [(or (bottom? x) (bottom? y)) bottom]
          [(top? x) y]
          [(top? y) x]
          [else ((lattice-meet lattice) x y)]))

  (define (bounded-lte? x y)
    (cond [(or (bottom? x) (top? y)) #t] ;; bottom <= all, all <= top
          [(or (top? x) (bottom? y)) #f] ;; this must be after the prev. case
          [else ((lattice-lte? lattice) x y)]))

  (define (bounded-comparable? x y)
    (or (top? x) (top? y)
        (bottom? x) (bottom? y)
        ((lattice-comparable? lattice) x y)))

  (define (bounded-comparable?-hash-code x [recur equal-hash-code])
    (if (or (top? x) (bottom? x))
        (recur x)
        ((lattice-comparable?-hash-code lattice) x recur)))

  (bounded-lattice bounded-join
                   bounded-gte?
                   bounded-meet
                   bounded-lte?
                   bounded-comparable?
                   bounded-comparable?-hash-code
                   top
                   bottom))

;; lexicographic-pointwise-semi-lattice : [X Y -> B]
;;                                        ([B -> X] [Semi-Lattice X])
;;                                        ([B -> Y] [Semi-Lattice Y])
;;                                        ->
;;                                        [Semi-Lattice B]
;;
;; In this case we use lexicographic ordering (1, 0) <= (1, 5) and (2, 1) <= (1, 5)
(define-syntax lexicographic-pointwise-semi-lattice/2
  (syntax-rules ()
    ((_ creator (accessor1 lattice1) (accessor2 lattice2))
     (let ()
       (define (lifted-join x y)
         (define gte1? (semi-lattice-gte? lattice1))
         (define x.1>=y.1 (gte1? (accessor1 x) (accessor1 y)))
         (define y.1>=x.1 (gte1? (accessor1 y) (accessor1 x)))
         (define x.1==y.1 (and x.1>=y.1 y.1>=x.1))
         (cond
          [x.1==y.1
           (creator (accessor1 x)
                    ((semi-lattice-join lattice2) (accessor2 x) (accessor2 y)))]
          [x.1>=y.1 x] ; the first component of x is strictly larger than "" "" y
          [y.1>=x.1 y] ; the first component of y is strictly larger than "" "" x
          ))
       (define (lifted-gte? x y)
         (define gte1? (semi-lattice-gte? lattice1))
         (define gte2? (semi-lattice-gte? lattice2))
         (define x.1>=y.1 (gte1? (accessor1 x) (accessor1 y)))
         (define y.1>=x.1 (gte1? (accessor1 y) (accessor1 x)))
         (define x.1==y.1 (and x.1>=y.1 y.1>=x.1))
         (if x.1==y.1
             (gte2? (accessor2 x) (accessor2 y))
             x.1>=y.1))
       (define (lifted-comparable? x y)
         (define gte1? (semi-lattice-gte? lattice1))
         (define gte2? (semi-lattice-gte? lattice2))
         (define comparable1? (semi-lattice-comparable? lattice1))
         (define comparable2? (semi-lattice-comparable? lattice2))
         (define x.1>=y.1 (gte1? (accessor1 x) (accessor1 y)))
         (define y.1>=x.1 (gte1? (accessor1 y) (accessor1 x)))
         (define x.1==y.1 (and x.1>=y.1 y.1>=x.1))
         (if x.1==y.1
             (comparable2? (accessor2 x) (accessor2 y))
             (comparable1? (accessor1 x) (accessor1 y))))
       (define (lifted-comparable?-hash-code x [recur equal-hash-code])
         ((semi-lattice-comparable?-hash-code lattice1) (accessor1 x) recur))
       (semi-lattice lifted-join
                     lifted-gte?
                     lifted-comparable?
                     lifted-comparable?-hash-code)))))

;; pointwise-semi-lattice : [A ... -> B]
;;                          ([B -> A] [Semi-Lattice A]) ...
;;                          ->
;;                          [Semi-Lattice B]
;;
;; Given a structure creator, structor accessors, and semi-lattices for each
;; accessor, `pointwise-semi-lattice' produces a point-wise lifted semi-lattice
;; over the structure.
;;
(define-syntax (pointwise-semi-lattice stx)
  (syntax-parse stx
    ((_:id creator:id (accessor:id lattice:expr) ...)
     #`(let ()
         #,(syntax/loc stx
             (define (lifted-join x y)
               (creator ((semi-lattice-join lattice) (accessor x)
                         (accessor y))
                        ...)))
         #,(syntax/loc stx
             (define (lifted-gte? x y)
               (and ((semi-lattice-gte? lattice) (accessor x)
                     (accessor y))
                    ...)))
         #,(syntax/loc stx
             (define (lifted-comparable? x y)
               (and ((semi-lattice-comparable? lattice) (accessor x)
                     (accessor y))
                    ...)))
         #,(syntax/loc stx
             (define (lifted-comparable?-hash-code x [recur equal-hash-code])
               (+ ((semi-lattice-comparable?-hash-code lattice) (accessor x) recur)
                  ...)))
         (semi-lattice lifted-join
                       lifted-gte?
                       lifted-comparable?
                       lifted-comparable?-hash-code)))))


;; pointwise-bounded-semi-lattice : [A ... -> B]
;;                                  ([B -> A] [Bounded-Semi-Lattice A]) ...
;;                                  ->
;;                                  [Bounded-Semi-Lattipce B]
;;
;; Given a structure creator, structor accessors, and bounded-semi-lattices for
;; each accessor, `pointwise-semi-lattice' produces a point-wise lifted
;; bounded-semi-lattice over the structure.
;;
(define-syntax pointwise-bounded-semi-lattice
  (syntax-rules ()
    ((_ creator (accessor lattice) ...)
     (let ()
       (define lifted-identity
         (creator (bounded-semi-lattice-identity lattice) ...))

       (bounded-semi-lattice-from-semi-lattice
        (pointwise-semi-lattice creator (accessor lattice) ...)
        lifted-identity)))))

;; pointwise-lattice : [A ... -> B]
;;                     ([B -> A] [Lattice A]) ...
;;                     ->
;;                     [Lattipce B]
;;
;; Given a structure creator, structor accessors, and lattices for each
;; accessor, `pointwise-semi-lattice' produces a point-wise lifted lattice over
;; the structure.
;;
(define-syntax (pointwise-lattice stx)
  (syntax-parse stx
    [(_:id creator:id (accessor:id lat:expr) ...)
     #`(let ()
         #,(syntax/loc stx
             (define (lifted-join x y)
               (creator ((lattice-join lat) (accessor x)
                         (accessor y))
                        ...)))
         #,(syntax/loc stx
             (define (lifted-gte? x y)
               (and ((lattice-gte? lat) (accessor x)
                     (accessor y))
                    ...)))
         #,(syntax/loc stx
             (define (lifted-meet x y)
               (creator ((lattice-meet lat) (accessor x)
                         (accessor y))
                        ...)))
         #,(syntax/loc stx
             (define (lifted-lte? x y)
               (and ((lattice-lte? lat) (accessor x)
                     (accessor y))
                    ...)))
         #,(syntax/loc stx
             (define (lifted-comparable? x y)
               (and ((lattice-comparable? lat) (accessor x)
                     (accessor y))
                    ...)))
         #,(syntax/loc stx
             (define (lifted-comparable?-hash-code x [recur equal-hash-code])
               (+ ((lattice-comparable?-hash-code lat) (accessor x) recur)
                  ...)))
         (lattice lifted-join
                  lifted-gte?
                  lifted-meet
                  lifted-lte?
                  lifted-comparable?
                  lifted-comparable?-hash-code))]))

;; pointwise-bounded-lattice : [A ... -> B]
;;                             ([B -> A] [Lattice A]) ...
;;                             ->
;;                             [Lattipce B]
;;
;; Given a structure creator, structor accessors, and bounded-lattices for each
;; accessor, `pointwise-semi-lattice' produces a point-wise lifted
;; bounded-lattice over the structure.
;;
(define-syntax pointwise-bounded-lattice
  (syntax-rules ()
    ((_ creator (accessor lattice) ...)
     (let ()
       (define lifted-top
         (creator (bounded-lattice-top lattice) ...))
       (define lifted-bottom
         (creator (bounded-lattice-bottom lattice) ...))

       (bounded-lattice-from-lattice
        (pointwise-lattice creator (accessor lattice) ...)
        lifted-top
        lifted-bottom)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disjoint Unions

;; lattice-disjoint-union : [Lattice A]
;;                          [Any -> Boolean]
;;                          [Lattice B]
;;                          [Any -> Boolean]
;;                          ->
;;                          [Lattice [U A B]]
;;
;; The two predicates are guards to ensure that values are only operated on with
;; the operate lattice operations.
;;
(define (lattice-disjoint-union left left? right right?)
  ;; disjoint-ops/default : X [A A -> X] [B B -> X] -> X
  ;;
  ;; An abstraction for choosing hte appropriate lattice operation baesd on the
  ;; predicates. Returns the default value if neither predicate holds on both
  ;; arguments
  ;;
  (define (disjoint-ops/default default left-op right-op)
    (lambda (x y)
      (cond [(and (left? x) (left? y)) (left-op x y)]
            [(and (right? x) (right? y)) (right-op x y)]
            [else default])))
  ;; disjoint-ops/err : Symbol [A A -> X] [B B -> Y] -> [U X Y]
  ;;
  ;; An abstraction for choosing the appropriate lattice operation based on the
  ;; predicates. Throws an error if neither predicates holds on both arguments
  ;;
  (define (disjoint-ops/err error-name left-op right-op)
    (lambda (x y)
      (cond [(and (left? x) (left? y)) (left-op x y)]
            [(and (right? x) (right? y)) (right-op x y)]
            [else
             (error error-name
                    (string-append
                     "must be given two elements, both from either the left or "
                     "right lattice; given ~a and ~a.")
                    x y)])))
  (define lifted-join
    (disjoint-ops/err 'lattice-disjoint-union-join
                      (lattice-join left)
                      (lattice-join right)))
  (define lifted-gte?
    (disjoint-ops/default #f
                          (lattice-gte? left)
                          (lattice-gte? right)))
  (define lifted-meet
    (disjoint-ops/err 'lattice-disjoint-union-meet
                      (lattice-meet left)
                      (lattice-meet right)))
  (define lifted-lte?
    (disjoint-ops/default #f
                          (lattice-lte? left)
                          (lattice-lte? right)))
  (define lifted-comparable?
    (disjoint-ops/default #f
                          (lattice-comparable? left)
                          (lattice-comparable? right)))
  (define lifted-comparable?-hash-code
    (disjoint-ops/default 0
                          (lattice-comparable?-hash-code left)
                          (lattice-comparable?-hash-code right)))
  (lattice lifted-join
           lifted-gte?
           lifted-meet
           lifted-lte?
           lifted-comparable?
           lifted-comparable?-hash-code))

(module+ test
  (let ()
    (define strings-and-numbers-lattice
      (lattice-disjoint-union lattice-on-numbers
                              number?
                              lattice-on-truth
                              boolean?))
    (define join (lattice-join strings-and-numbers-lattice))
    (define gte? (lattice-gte? strings-and-numbers-lattice))
    (define meet (lattice-meet strings-and-numbers-lattice))
    (define lte? (lattice-lte? strings-and-numbers-lattice))
    ;; The next eight sections of checks are from the primitive-lattices.rkt
    ;; truth join
    (check-true (join #t #t))
    (check-true (join #t #f))
    (check-true (join #f #t))
    (check-false (join #f #f))
    ;; truth gte?
    (check-true (gte? #t #t))
    (check-true (gte? #t #f))
    (check-false (gte? #f #t))
    (check-true (gte? #f #f))
    ;; truth meet
    (check-true (meet #t #t))
    (check-false (meet #t #f))
    (check-false (meet #f #t))
    (check-false (meet #f #f))
    ;; truth lte?
    (check-true (lte? #t #t))
    (check-false (lte? #t #f))
    (check-true (lte? #f #t))
    (check-true (lte? #f #f))
    ;; number join
    (check-equal? (join 0 1) 1)
    (check-equal? (join -1 1) 1)
    (check-equal? (join 10 100) 100)
    ;; number gte?
    (check-false (gte? 0 1))
    (check-true (gte? 0 0))
    (check-true (gte? 0 -1))
    (check-true (gte? 100 10))
    ;; number meet
    (check-equal? (meet 0 1) 0)
    (check-equal? (meet -1 1) -1)
    (check-equal? (meet 10 100) 10)
    ;; number lte?
    (check-true (lte? 0 1))
    (check-true (lte? 0 0))
    (check-false (lte? 0 -1))
    (check-false (lte? 100 10))
    ;; combinations
    (for ((op (list gte? lte?))
          (num (list 10 -10))
          (bool (list #t #f)))
      (check-false (op num bool))
      (check-false (op bool num)))
    (for ((op (list join meet))
          (num (list 10 -10))
          (bool (list #t #f)))
      (check-exn exn:fail? (lambda () (op num bool)))
      (check-exn exn:fail? (lambda () (op bool num))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linear Sums

;; TODO


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dictionaries

;; dictionary-merge : [Dict K V] [Dict K V] [V V -> V] V -> [Dict K V]
;;
;; Produces a dictionary which maps a given key to f(dict1(key), dict2(key)).
;; It must be the case that f(x, identity) = x.
(define (dictionary-merge dict1 dict2 f identity)
  (if (dict-can-functional-set? dict1)
      (for/fold ([new-dict dict1])
          ([(k v) (in-dict dict2)])
        (dict-set new-dict
                  k
                  (f v (dict-ref new-dict k identity))))
      (for/fold ([new-dict (dict-copy dict1)])
          ([(k v) (in-dict dict2)])
        (dict-set! new-dict
                   k
                   (f v (dict-ref new-dict k identity))))))

;; dictionary-andmap2 : [Dict K V] [Dict K V] [V -> Boolean] V -> Boolean
;;
;; This is used to point-wise lift predicates on values to predicates on
;; dictionaries where only values with equivalent keys are compared
;;
(define (dictionary-andmap2 dict1 dict2 predicate default)
  (for/and ([key (sequence-append (in-dict-keys dict1)
                                  (in-dict-keys dict2))])
    (predicate (dict-ref dict1 key default) (dict-ref dict2 key default))))

;; all-keys [Dict K V] [Dict K V] -> [SetOf K]
;;
;; Produces all the keys that are present in at least one of the given
;; dictionaries.
(define (all-keys d1 d2)
  (set-union (list->set (dict-keys d1)) (list->set (dict-keys d2))))

;; TODO this should be parameterized over all lattices
;; TODO optionally provide bottom and top values?
;;
;; make-bounded-dictionary-lattice : [Bounded-Lattice V]
;;                                   ->
;;                                   ∃Top.∃Bottom.
;;                                   [Bounded-Lattice [Dict K V]]
;;                                   Top
;;                                   [Any -> Boolean]
;;                                   Bottom
;;                                   [Any -> Boolean]
;;
;; Produces a bounded-lattice on dictionaries given a lattice on their
;; domain. The join operator is point-wise defined as
;;
;;   (d1 join d2)(x) = d1(x) join d2(x)
;;
;; The less than or equal to operator is point-wise defiend as
;;
;;   d1 <= d2 = ∀x. d1(x) <= d2(x)
;;
;; Partial dictionaries are implicitly treated as total dictionaries by assuming
;; unmapped keys map to the appropriate identity element for the operation
;; (bottom for join and top for meet)
;;
;; Dictionaries are comparable if, forall k, d1(k) is comparable to d2(k)
;;
;; Special top and bottom values are produced which implement gen:dict and map
;; all keys to top or bottom, respectively. The returned predicates are
;; predicates for those values.
;;
(define (make-bounded-dictionary-lattice value-lattice)
  (define (dictionary-join dict1 dict2)
    (dictionary-merge dict1
                      dict2
                      (lattice-join value-lattice)
                      (bounded-lattice-bottom value-lattice)))
  (define (dictionary-gte? dict1 dict2)
    (or (eq? dict1 dict2)
        (dictionary-andmap2 dict1 dict2
                            (lattice-gte? value-lattice)
                            (bounded-lattice-bottom value-lattice))))
  (define (dictionary-meet dict1 dict2)
    (dictionary-merge dict1
                      dict2
                      (lattice-meet value-lattice)
                      (bounded-lattice-top value-lattice)))
  (define (dictionary-lte? dict1 dict2)
    (or (eq? dict1 dict2)
        (dictionary-andmap2 dict1 dict2
                            (lattice-lte? value-lattice)
                            (bounded-lattice-bottom value-lattice))))
  (define (dictionary-comparable? dict1 dict2)
    (or (dictionary-gte? dict1 dict2)
        (dictionary-lte? dict1 dict2)))
  (define (dictionary-comparable?-hash-code dict1 [recur equal-hash-code])
    (for/sum ([(k v) dict1])
      ((lattice-comparable?-hash-code value-lattice) v recur)))
  (define-default-value-dict dict-top (bounded-lattice-top value-lattice))
  ;; (define-default-value-dict dict-bottom (bounded-lattice-bottom value-lattice))
  (values (bounded-lattice dictionary-join
                           dictionary-gte?
                           dictionary-meet
                           dictionary-lte?
                           dictionary-comparable?
                           dictionary-comparable?-hash-code
                           (dict-top)
                           (hash))
          (dict-top)
          dict-top?
          (hash)
          (lambda (x)
            (and (hash? x)
                 (= (hash-count x) 0)))))
(module+ test
  (define-values
    (bounded-lattice-on-numbers blon-top blon-top? blon-bottom blon-bottom?)
    (make-bounded-lattice lattice-on-numbers))
  (define-values
    (bounded-lattice-on-number-domain-dicts
     blondd-top blondd-top? blondd-bottom blondd-bottom?)
    (make-bounded-dictionary-lattice bounded-lattice-on-numbers))
  (define join-number-domain-dicts
    (lattice-join bounded-lattice-on-number-domain-dicts))
  (define meet-number-domain-dicts
    (lattice-meet bounded-lattice-on-number-domain-dicts))
  (define gte?-number-domain-dicts
    (lattice-gte? bounded-lattice-on-number-domain-dicts))
  (define lte?-number-domain-dicts
    (lattice-lte? bounded-lattice-on-number-domain-dicts))
  (define comparable?-number-domain-dicts
    (lattice-comparable? bounded-lattice-on-number-domain-dicts))
  (define squares (hash 0.25 0.0625  0.5 0.25  1 1  2 4  3 9  4 16  5 25))
  (define doubles (hash 0.25 0.5     0.5 1.0   1 1  2 4  3 6  4 8   5 10))
  (define bigger  (join-number-domain-dicts squares doubles))
  (define smaller (meet-number-domain-dicts squares doubles))
  (check-equal? bigger
                (hash 0.25 0.5  0.5 1.0  1 1  2 4  3 9  4 16  5 25))
  (check-equal? smaller
                (hash 0.25 0.0625  0.5 0.25  1 1  2 4  3 6  4 8  5 10))
  (check-true (blon-top? (dict-ref blondd-top 1)))
  (check-true (empty? (dict-keys blondd-bottom)))
  (check-true (gte?-number-domain-dicts blondd-bottom (hash 1 blon-bottom 2 blon-bottom)))
  (check-true (gte?-number-domain-dicts (hash 1 blon-bottom 2 blon-bottom) blondd-bottom))
  ;; sanity checks on predicates
  (check-true (blondd-top? blondd-top))
  (check-true (blondd-bottom? blondd-bottom))
  (check-false (blondd-bottom? blondd-top))
  (check-false (blondd-top? blondd-bottom))
  ;; bigger is the join of the squares and doubles
  (check-true (gte?-number-domain-dicts bigger squares))
  (check-true (gte?-number-domain-dicts bigger doubles))
  ;; bigger is strictly larger than the other two
  (check-false (gte?-number-domain-dicts squares bigger))
  (check-false (gte?-number-domain-dicts doubles bigger))
  ;; smaller is the meet of the squares and doubles
  (check-true (lte?-number-domain-dicts smaller squares))
  (check-true (lte?-number-domain-dicts smaller doubles))
  ;; smaller is strictly smaller
  (check-false (lte?-number-domain-dicts squares smaller))
  (check-false (lte?-number-domain-dicts doubles smaller))
  ;; let's directly test comparability
  (check-true (comparable?-number-domain-dicts squares smaller))
  (check-true (comparable?-number-domain-dicts doubles smaller))
  (check-true (comparable?-number-domain-dicts squares bigger))
  (check-true (comparable?-number-domain-dicts doubles bigger))
  ;; squares and doubles should be incomparable
  (check-false (comparable?-number-domain-dicts squares doubles))
  ;; top and bottom should always be comaprable
  (check-true (comparable?-number-domain-dicts blondd-top squares))
  (check-true (comparable?-number-domain-dicts blondd-bottom squares))
  (check-true (comparable?-number-domain-dicts blondd-top doubles))
  (check-true (comparable?-number-domain-dicts blondd-bottom doubles))
  ;; is top really top?
  (check-true (gte?-number-domain-dicts blondd-top squares))
  (check-true (gte?-number-domain-dicts blondd-top doubles))
  (check-true (lte?-number-domain-dicts squares blondd-top))
  (check-true (lte?-number-domain-dicts doubles blondd-top))
  ;; is top identity for meet?
  (check-equal? (meet-number-domain-dicts squares blondd-top) squares)
  (check-equal? (meet-number-domain-dicts doubles blondd-top) doubles)
  ;; is bottom really bottom?
  (check-true (lte?-number-domain-dicts blondd-bottom squares))
  (check-true (lte?-number-domain-dicts blondd-bottom doubles))
  (check-true (gte?-number-domain-dicts squares blondd-bottom))
  (check-true (gte?-number-domain-dicts doubles blondd-bottom))
  ;; is bottom identity for join?
  (check-equal? (join-number-domain-dicts squares blondd-bottom) squares)
  (check-equal? (join-number-domain-dicts doubles blondd-bottom) doubles)
  ;; bottom should be equivalent to the empty dict
  ;; (check-equal? blondd-bottom (hash)) ;; this fails...
  (check-true (gte?-number-domain-dicts blondd-bottom (hash)))
  (check-true (gte?-number-domain-dicts (hash) blondd-bottom)))

;; produces a "fake" dictionary which simply maps to the def-value and has the
;; structure name name
(define-syntax define-default-value-dict
  (syntax-rules ()
    ((_ name def-value)
     (struct name ()
             #:methods gen:dict
             [(define (dict-ref dict key [default #f])
                def-value)
              (define (dict-set dict key val)
                ;; TODO is there a way to avoid hard-coding a type of dict here?
                (hash key val))
              (define (dict-remove dict key)
                dict)
              (define (dict-count dict #:default [x #f])
                0)
              (define (dict-iterate-first dict)
                #f)
              (define (dict-iterate-next dict pos)
                #f)
              (define (dict-iterate-key dict pos)
                (raise
                 (exn:fail:contract
                  "no keys (this dict was produced by a lattice operation)"
                  (current-continuation-marks))))
              (define (dict-iterate-value dict pos)
                (raise
                 (exn:fail:contract
                  "no values (this dict was produced by a lattice operation)"
                  (current-continuation-marks))))]))))

