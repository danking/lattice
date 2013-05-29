#lang racket

(require "data.rkt")
(provide construct-lattice-from-semi-lattices
         construct-bounded-lattice-from-semi-lattices
         get-join-semi-lattice-from-lattice
         get-meet-semi-lattice-from-lattice
         make-bounded-lattice
         make-bounded-semi-lattice
         pointwise-semi-lattice
         pointwise-bounded-semi-lattice
         pointwise-lattice
         pointwise-bounded-lattice
         make-bounded-dictionary-lattice)

(module+ test (require rackunit "primitive-lattices.rkt"))

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
(define (make-bounded-semi-lattice lattice)
  ;; prefabs wouldn't work here because prefabs rely on unique names
  (struct identity ())

  (define (bounded-join x y)
    (cond [(identity? x) y]
          [(identity? y) x]
          [else ((lattice-join lattice) x y)]))

  (define (bounded-gte? x y)
    (cond [(identity? y) #t]  ;; everything >= identity
          [(identity? x) #f]  ;; identity >= nothing
          [else ((lattice-gte? lattice) x y)]))

  (define (bounded-comparable? x y)
    (or (identity? x) (identity? y)
        ((lattice-comparable? lattice) x y)))

  (define (bounded-comparable?-hash-code x [recur equal-hash-code])
    (if (or (identity? x))
        (recur x)
        ((lattice-comparable?-hash-code lattice) x recur)))

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
    (cond [(or (bottom? x) (bottom? y)) top]
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

  (values (bounded-lattice bounded-join
                           bounded-gte?
                           bounded-meet
                           bounded-lte?
                           bounded-comparable?
                           bounded-comparable?-hash-code
                           (top)
                           (bottom))
          (top) top? (bottom) bottom?))

;; pointwise-semi-lattice : [A ... -> B]
;;                          ([B -> A] [Semi-Lattice A]) ...
;;                          ->
;;                          [Semi-Lattipce B]
;;
;; Given a structure creator, structor accessors, and semi-lattices for each
;; accessor, `pointwise-semi-lattice' produces a point-wise lifted semi-lattice
;; over the structure.
;;
(define-syntax pointwise-semi-lattice
  (syntax-rules ()
    ((_ creator (accessor lattice) ...)
     (let ()
       (define (lifted-join x y)
         (creator ((semi-lattice-join lattice) (accessor x)
                                               (accessor y))
                  ...))
       (define (lifted-gte? x y)
         (and ((semi-lattice-gte? lattice) (accessor x)
                                           (accessor y))
              ...))
       (define (lifted-comparable? x y)
         (and ((semi-lattice-comparable? lattice) (accessor x)
                                                  (accessor y))
              ...))
       (define (lifted-comparable?-hash-code x [recur equal-hash-code])
         (+ ((semi-lattice-comparable?-hash-code lattice) (accessor x) recur)
            ...))
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
(define-syntax pointwise-lattice
  (syntax-rules ()
    ((_ creator (accessor lat) ...)
     (let ()
       (define (lifted-join x y)
         (creator ((lattice-join lat) (accessor x)
                                               (accessor y))
                  ...))
       (define (lifted-gte? x y)
         (and ((lattice-gte? lat) (accessor x)
                                           (accessor y))
              ...))
       (define (lifted-meet x y)
         (creator ((lattice-meet lat) (accessor x)
                                               (accessor y))
                  ...))
       (define (lifted-lte? x y)
         (and ((lattice-lte? lat) (accessor x)
                                           (accessor y))
              ...))
       (define (lifted-comparable? x y)
         (and ((lattice-comparable? lat) (accessor x)
                                                  (accessor y))
              ...))
       (define (lifted-comparable?-hash-code x [recur equal-hash-code])
         (+ ((lattice-comparable?-hash-code lat) (accessor x) recur)
            ...))
       (lattice lifted-join
                lifted-gte?
                lifted-meet
                lifted-lte?
                lifted-comparable?
                lifted-comparable?-hash-code)))))

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
;; Dictionaries

;; dictionary-merge : [Dict K V] [Dict K V] [V V -> V] V -> [Dict K V]
;;
;; Produces a dictionary which maps a given key to f(dict1(key), dict2(key)).
;; It must be the case that f(x, identity) = x.
(define (dictionary-merge dict1 dict2 f identity)
  (for/fold ([new-dict dict1])
            ([(k v) (in-dict dict2)])
    (dict-set new-dict
              k
              (f v (dict-ref new-dict k identity)))))

;; dictionary-andmap2 : [Dict K V] [Dict K V] [V -> Boolean] -> Boolean
;;
;; This is used to point-wise lift predicates on values to predicates on
;; dictionaries where only values with equivalent keys are compared
;;
(define (dictionary-andmap2 dict1 dict2 predicate)
  (for/and ([key (in-set (all-keys dict1 dict2))])
    (predicate (dict-ref dict1 key) (dict-ref dict2 key))))

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
    (dictionary-andmap2 dict1 dict2 (lattice-gte? value-lattice)))
  (define (dictionary-meet dict1 dict2)
    (dictionary-merge dict1
                      dict2
                      (lattice-meet value-lattice)
                      (bounded-lattice-top value-lattice)))
  (define (dictionary-lte? dict1 dict2)
    (dictionary-andmap2 dict1 dict2 (lattice-lte? value-lattice)))
  (define (dictionary-comparable? dict1 dict2)
    (or (dictionary-gte? dict1 dict2)
        (dictionary-lte? dict1 dict2)))
  (define (dictionary-comparable?-hash-code dict1 [recur equal-hash-code])
    (for/sum ([(k v) dict1])
      ((lattice-comparable?-hash-code value-lattice) v recur)))
  (define-default-value-dict dict-top (bounded-lattice-top value-lattice))
  (define-default-value-dict dict-bottom (bounded-lattice-bottom value-lattice))
  (values (bounded-lattice dictionary-join
                           dictionary-gte?
                           dictionary-meet
                           dictionary-lte?
                           dictionary-comparable?
                           dictionary-comparable?-hash-code
                           (dict-top)
                           (dict-bottom))
          (dict-top)
          dict-top?
          (dict-bottom)
          dict-bottom?))
(module+ test
  ;; lattice on numbers (i.e. (construct-lattice...)) is defined in
  ;; derived-lattices.rkt, but requiring that file here would cause a circular
  ;; depdendcy
  (define-values
    (bounded-lattice-on-numbers blon-top blon-top? blon-bottom blon-bottom?)
    (make-bounded-lattice
     (construct-lattice-from-semi-lattices ascending-semi-lattice-on-numbers
                                           descending-semi-lattice-on-numbers)))
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
  (check-true (blon-bottom? (dict-ref blondd-bottom 1)))
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

