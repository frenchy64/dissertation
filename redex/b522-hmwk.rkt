#lang racket
(require redex)

;; CSCI-B-522 Homework 2

;; λ-calculus
(define-language lc
  [X ::= variable-not-otherwise-mentioned]
  [E ::= X (λ (X) E) (E E)]
  [C ::= hole (λ (X) C) (C E) (E C)])

;; fv
;; E -> (listof X)
;; calculates the free variables
;; that appear in a λ-calculus term
(define-metafunction lc
  fv : E -> (X ...)
  [(fv X) (X)]
  [(fv (λ (X) E)) ,(remove* (term (X)) (term (fv E)))]
  [(fv (E_1 E_2))
   (X_1 ... X_2 ...)
   (where (X_1 ...) (fv E_1))
   (where (X_2 ...) (fv E_2))])

;; fv tests
(module+ test
  ;; check fv var case
  (test-equal (term (fv x))
              (term (x)))
  ;; λ case
  ;; check bound var is excluded
  (test-equal (term (fv (λ (x) (x y))))
              (term (y)))
  ;; check fv app case
  (test-equal (term (fv (x y)))
              (term (x y))
              #:equiv set=?)
  ;; non-trivial/compound test
  (test-equal (term (fv ((λ (a) (b c))
                         (λ (d) (d e)))))
              (term (b c e))
              #:equiv set=?))


;; bv
;; e -> (listof x)
;; calculates the bound variables
;; that appear in a λ-calculus term
(define-metafunction lc
  bv : E -> (X ...)
  [(bv E) (bv* E ())])

;; bv*
;; helper for bv, passes along list of currently bound variables
(define-metafunction lc
  bv* : E (X ...) -> (X ...)
  [(bv* X_i (X_0 ... X_i X_i+1 ...)) (X_i)]
  [(bv* X any) ()]
  [(bv* (λ (X) E) (X_0 ...)) (bv* E (X X_0 ...))]
  [(bv* (E_1 E_2) any_bound)
   ,(set-union (term (bv* E_1 any_bound))
               (term (bv* E_2 any_bound)))])

;; bv tests
(module+ test
  ;; check bv var case
  (test-equal (term (bv x))
              (term ()))
  ;; λ case
  ;; check bound var is included if found
  ;; in the body
  (test-equal (term (bv (λ (x) x)))
              (term (x)))
  (test-equal (term (bv (λ (x) y)))
              (term ()))
  ;; check fv app case
  (test-equal (term (bv ((λ (x) x)
                         (λ (y) y))))
              (term (x y))
              #:equiv set=?)
  ;; non-trivial/compound test
  (test-equal (term (bv ((λ (a) (b c))
                         (λ (d) (d e)))))
              (term (d))
              #:equiv set=?))


;; subst
(define-metafunction lc
  subst : E [X ↦ E] -> E
  [(subst X [X ↦ E]) E]
  [(subst X_1 [X ↦ E]) X_1]
  [(subst (λ (X_1) E_1) [X ↦ E])
   (λ (X_ν) (subst (subst E_1 [X_1 ↦ X_ν]) [X ↦ E]))
   (where X_ν ,(variable-not-in (term (X E_1 E)) (term X_1)))]
  [(subst (E_1 E_2) [X ↦ E])
   ((subst E_1 [X ↦ E])
    (subst E_2 [X ↦ E]))])

;; α=?
(define-metafunction lc
  α=? : E E -> boolean
  [(α=? X_1 X_2) ,(equal? (term X_1) (term X_2))]
  [(α=? (λ (X_1) E_1)
        (λ (X_2) E_2))
   (α=? E_1 (subst E_2 [X_2 ↦ X_1]))]
  [(α=? (E_1l E_1r) (E_2l E_2r))
   ,(and (term (α=? E_1l E_2l))
         (term (α=? E_1r E_2r)))]
  [(α=? E_1 E_2) #f])

(define (α-equal? t1 t2)
  (term (α=? ,t1 ,t2)))

;; α= and subst tests
(module+ test
  ;; subst var tests
  (test-equal (term (subst x [x ↦ y]))
              (term y))
  (test-equal (term (subst x [y ↦ z]))
              (term x))
  
  ;; subst λ tests
  ;; verify substitution respects binders introducing
  ;; names being substituted for
  (test-equal
   (term (subst (λ (x) x) [x ↦ y]))
   (term (λ (z) z))
   #:equiv α-equal?)
  
  ;; verify substitution does not capture variables
  (test-equal
   (term (subst (λ (x) (x y)) [y ↦ (x x)]))
   (term (λ (z) (z (x x))))
   #:equiv α-equal?)

  
  ;; subst app tests
  (test-equal (term (subst ((x y) (x y)) [x ↦ y]))
              (term ((y y) (y y))))
  
  ;; α=? var tests
  (test-equal (term (α=? x x))
              #t)
  (test-equal (term (α=? x y))
              #f)
  
  ;; α=? subst λ tests
  (test-equal (term (α=? (λ (a) (a x))
                         (λ (b) (b x))))
              #t)
  ;; α=? app tests
  (test-equal (term (α=? ((λ (a) (a x))
                          (λ (b) (b x)))
                         ((λ (c) (c x))
                          (λ (d) (d x)))))
              #t)
  )

;; -->βη
;; reduction relation
;; *only use β and η reductions*
;; α-equivalence can then be used when testing -->βη
(define -->βη
  (reduction-relation
   lc
   #:domain E
   [--> (in-hole C ((λ (X) E_1) E_2))
        (in-hole C (subst E_1 [X ↦ E_2]))
        "β"]
   [--> (in-hole C (λ (X) (E_1 X)))
        (in-hole C E_1)
        (where #f ,(set-member? (term (fv E_1)) (term X)))
        "η"]))


;; -->βη tests
(module+ test
  ;; test for var (no reduction)
  (test--> -->βη
           #:equiv α-equal?
           (term x))
  ;; test for lambda (no reduction)
  (test--> -->βη
           #:equiv α-equal?
           (term (λ (x) x)))
  ;; β
  (test--> -->βη
           #:equiv α-equal?
           (term ((λ (x) x) y))
           (term y))
  ;; β in a context
  (test--> -->βη
           #:equiv α-equal?
           (term (((λ (x) x) y) z))
           (term (y z)))
  ;; η
  (test--> -->βη
           #:equiv α-equal?
           (term (λ (x) (f x)))
           (term f))
  
  ;; η in a context
  (test--> -->βη
           #:equiv α-equal?
           (term ((λ (x) x) (λ (x) (f x))))
           (term (λ (x) (f x)))
           (term ((λ (x) x) f)))
  ;; no η only if X ∈ fv(E)
  (test--> -->βη
           #:equiv α-equal?
           (term (λ (x) ((x x) x)))))

