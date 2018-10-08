#lang racket

(require redex)
(require racket/random)
(require racket/format)
(require test-engine/racket-tests)
(require redex/pict)
(require pict)
; uncomment this and add-atomic-rewriters! for pretty printing
(require unstable/gui/redex)

(define mf-font "Latin Modern Mono")
(define math-font "Latin Modern Math")
(define lit-font "Latin Modern Mono Caps")
(default-font-size 10)
(label-font-size 10)
(metafunction-font-size 10)
(non-terminal-style "Latin Modern Math")
(non-terminal-subscript-style (cons 'subscript "Latin Modern Math"))
(non-terminal-superscript-style (cons 'superscript "Latin Modern Math"))
(default-style "Latin Modern Math")
(literal-style "Latin Modern Math")
(paren-style "Latin Modern Math")
(grammar-style "Latin Modern Math")
(rule-pict-style 'horizontal)

(add-atomic-rewriters!
 'CNST    (λ () (text "C" math-font (default-font-size)))
 'P?    (λ () (text "P" math-font (default-font-size)))
 'M    (λ () (text "E" math-font (default-font-size)))
 '+      (λ () (text "+" mf-font (default-font-size)))
 '-      (λ () (text "-" mf-font (default-font-size)))
 '*      (λ () (text "*" mf-font (default-font-size)))
 'nil      (λ () (text "nil" math-font (default-font-size)))
 'true      (λ () (text "true" math-font (default-font-size)))
 'false      (λ () (text "false" math-font (default-font-size)))
 'C (lambda () (text "ℂ" mf-font (default-font-size)))
 'NT (lambda () (text "ℤ" mf-font (default-font-size)))
 'VA (lambda () (text "V" math-font (default-font-size)))
 'V (lambda () (hbl-append (text "V" math-font (default-font-size))
                           (text "e" (cons 'superscript math-font) (default-font-size))))
 'SP (lambda () (text "𝕊" math-font (default-font-size)))
 
 )



;; for generators
(caching-enabled? #f)

(define-language Clojure
  ;; expressions
  (M ::=
     CNST L X
     (M M ...)
     (if M M M))
  ; simple expressions with no free variables
  (CNST ::= N O B nil H ERR)
  (X ::= variable-not-otherwise-mentioned)
  (ERR ::= (error any any ...))
  (L ::= (fn [X ...] M) (fn X [X ...] M))
  (NONFNV ::= B H nil N)
  ; non-error values
  (VA ::= O L NONFNV)
  (V ::= VA ERR)
  (H ::= (HashMap (VA VA) ...))
  (B ::= true false)
  (N ::= number)
  (NT ::= natural)
  (O ::= P?
     inc dec 
     + * dissoc
     assoc get)
  (P? ::= zero? number? boolean? nil?)
  ; context
  (C ::= hole (if C M M) (VA ... C M ...)))

(define-language REDEX)
(define-judgment-form REDEX
  #:mode (lookup I I O)
  #:contract (lookup (any (any any) ...) any any)
  [(lookup (any_n _ ... (any any_0) _ ...) any any_0)])

(define-metafunction REDEX
  ext1 : (any (any any) ...) (any any) -> (any (any any) ...)
  [(ext1 (any_n any_0 ... (any_k any_v0) any_1 ...) (any_k any_v1))
   (any_n any_0 ... (any_k any_v1) any_1 ...)]
  [(ext1 (any_n any_0 ...) (any_k any_v1))
   (any_n (any_k any_v1) any_0 ...)])

(define-metafunction REDEX
  ext : (any (any any) ...) (any any) ... -> (any (any any) ...)
  [(ext any) any]
  [(ext any any_0 any_1 ...)
   (ext1 (ext any any_1 ...) any_0)])

(define-relation REDEX
  unique ⊆ (any ...)
  [(unique (any_!_1 ...))])

(define (remove-key H V_k)
  (list* 'HashMap
         (filter (lambda (e)
                   (not (equal? V_k (car e))))
                 (cdr H))))

(define (lookup-hash-map H V_k V_d)
  (let ([e (assv V_k (cdr H))])
    (if e
        (cadr e)
        V_d)))

;; fv
;; E -> (listof X)
;; calculates the free variables
;; that appear in a λ-calculus term
(define-metafunction Clojure
  fv : M -> (X ...)
  [(fv CNST) ()]
  [(fv X) (X)]
  [(fv (fn [X ...] M)) ,(remove* (term (X ...)) (term (fv M)))]
  [(fv (fn X_rec [X ...] M)) ,(remove* (term (X_rec X ...)) (term (fv M)))]
  [(fv (M M_args ...))
   ,(append-map (lambda (m) (term (fv ,m))) (term (M M_args ...)))])

;; fv tests
(module+ test
  ;; constants
  (test-equal (term (fv (fn [x]
                            (nil true false (HashMap)
                                 (error msg)
                                 inc
                                 +
                                 y
                                 assoc
                                 x
                                 1))))
              (term (y)))
  ;; check fv var case
  (test-equal (term (fv x))
              (term (x)))
  ;; λ case
  ;; check bound var is excluded
  (test-equal (term (fv (fn [x] (x y))))
              (term (y)))
  (test-equal (term (fv (fn rec [x] (x y))))
              (term (y)))
  ;; check fv app case
  (test-equal (term (fv (x y)))
              (term (x y))
              #:equiv set=?)
  ;; non-trivial/compound test
  (test-equal (term (fv ((fn [a] (b c))
                         (fn [d] (d e)))))
              (term (b c e))
              #:equiv set=?))

(define-judgment-form Clojure
  #:mode (δ I O)
  #:contract (δ (O VA ...) V)
  ;; TODO dynamic checks for map-ops
  [(δ (dissoc H V_k) ,(remove-key (term H) (term V_k)))]
  [(δ (assoc H V_k V_v) (ext H (V_k V_v)))]
  [(δ (get H V_k V_d) ,(lookup-hash-map (term H) (term V_k) (term V_d)))]
  [(δ (+ any_0 any_1) ,(cond
                         [(redex-match? Clojure (N_0 N_1) (term (any_0 any_1)))
                          (+ (term any_0) (term any_1))]
                         [else (term (error "Bad arguments to +" any_0 any_1))]))]
  [(δ (* any_0 any_1) ,(cond
                         [(redex-match? Clojure (N_0 N_1) (term (any_0 any_1)))
                          (* (term any_0) (term any_1))]
                         [else (term (error "Bad arguments to *" any_0 any_1))]))]
  [(δ (number? any) ,(if (number? (term any))
                         (term true)
                         (term false)))]
  [(δ (boolean? any) ,(if (or (equal? 'true (term any))
                              (equal? 'false (term any)))
                          (term true)
                          (term false)))]
  [(δ (nil? any) ,(if (equal? 'nil (term any))
                      (term true)
                      (term false)))]
  [(δ (dec any) ,(cond
                   [(redex-match? Clojure N (term any))
                    (sub1 (term any))]
                   [else (term (error "Bad arguments to dec" any))]))]
  [(δ (inc any) ,(cond
                   [(redex-match? Clojure N (term any))
                    (add1 (term any))]
                   [else (term (error "Bad arguments to inc" any))]))]
  [(δ (zero? any) ,(if (equal? 0 (term any))
                       (term true)
                       (term false)))]
  
  )

(define (truthy? v)
  (and (not (equal? v 'false))
       (not (equal? v 'nil))))

;; subst
(define-metafunction Clojure
  subst : M [X ↦ M] -> M
  [(subst X [X ↦ M]) M]
  [(subst X_1 [X ↦ M]) X_1]
  ;; subst X_1 to X_n
  [(subst (fn [X_1 ...] M_1) [X ↦ M])
   ,(let ([X_vs (map (lambda (x)
                       (variable-not-in (term (X M_1 M)) (term x)))
                     (term (X_1 ...)))])
      (term (fn ,X_vs
                (subst
                 (subst* M_1 ,(map (lambda (l r) (term [,l ↦ ,r]))
                                   (term (X_1 ...))
                                   X_vs))
                 [X ↦ M]))))]
  ;; subst X_rec first, then X_1 to X_n
  [(subst (fn X_rec [X_1 ...] M_1) [X ↦ M])
   ,(let* ([not-in (term (X X_rec M_1 M))]
           [X_vs (map (lambda (x)
                        (variable-not-in not-in (term x)))
                      (term (X_1 ...)))]
           [X_vrec (variable-not-in not-in (term X_rec))])
      (term (fn ,X_vrec
                ,X_vs
                (subst
                 (subst* M_1 ,(map (lambda (l r) (term [,l ↦ ,r]))
                                   (term (X_rec X_1 ...))
                                   (cons X_vrec X_vs)))
                 [X ↦ M]))))]
  [(subst (M_1 M_2 ...) [X ↦ M])
   ((subst M_1 [X ↦ M])
    (subst M_2 [X ↦ M]) ...)]
  [(subst (if M_0 M_1 M_2) [X ↦ M])
   (if (subst M_0 [X ↦ M])
       (subst M_1 [X ↦ M])
       (subst M_2 [X ↦ M]))]
  [(subst CNST [X ↦ M]) CNST])

(define-metafunction Clojure
  subst* : M ([X ↦ M] ...) -> M
  [(subst* M_1 ([X ↦ M] ...))
   ,(foldl (lambda (x_1 x_v m)
             (term (subst ,m [,x_1 ↦ ,x_v])))
           (term M_1)
           (term (X ...))
           (term (M ...)))])

;; α=?
(define-metafunction Clojure
  α=? : M M -> boolean
  [(α=? X_1 X_2) ,(equal? (term X_1) (term X_2))]
  [(α=? (fn (X_1 ...) M_1)
        (fn (X_2 ...) M_2))
   ,(and (equal? (length (term (X_1 ...)))
                 (length (term (X_2 ...))))
         (term (α=? M_1 (subst* M_2 ([X_2 ↦ X_1] ...)))))]
  [(α=? (fn X_rec1 (X_1 ...) M_1)
        (fn X_rec2 (X_2 ...) M_2))
   ,(and (equal? (length (term (X_1 ...)))
                 (length (term (X_2 ...))))
         (term (α=? M_1 (subst* M_2 ([X_rec2 ↦ X_rec1] [X_2 ↦ X_1] ...)))))]
  [(α=? (M_1l M_1r ...) (M_2l M_2r ...))
   ,(and (equal? (length (term (M_1r ...)))
                 (length (term (M_2r ...))))
         (term (α=? M_1l M_2l))
         (andmap (lambda (l r)
                   (term (α=? ,l ,r)))
                 (term (M_1r ...))
                 (term (M_2r ...))))]
  [(α=? V V) #t]
  [(α=? M_1 M_2) #f])

(define (α-equal? t1 t2)
  (term (α=? ,t1 ,t2)))

;; α=? and subst tests
(module+ test
  ;; subst var tests
  (test-equal (term (subst x [x ↦ y]))
              (term y))
  (test-equal (term (subst x [y ↦ z]))
              (term x))

  ;; subst* tests
  (test-equal
   (term (subst* (fn (x) x) ([x ↦ y])))
   (term (fn (z) z))
   #:equiv α-equal?)

  (test-equal
   (term (subst (b x) (b ↦ a)))
   (term (a x)))
  (test-equal
   (term (subst* (b x) ((b ↦ a))))
   (term (a x)))
  
  ;; subst λ tests
  ;; verify substitution respects binders introducing
  ;; names being substituted for
  (test-equal
   (term (subst (fn (x) x) [x ↦ y]))
   (term (fn (z) z))
   #:equiv α-equal?)
  (test-equal
   (term (subst (fn a (x) x) [x ↦ y]))
   (term (fn b (z) z))
   #:equiv α-equal?)
  
  ;; verify substitution does not capture variables
  (test-equal
   (term (subst (fn (x) (x y)) [y ↦ (x x)]))
   (term (fn (z) (z (x x))))
   #:equiv α-equal?)

  (test-equal
   (term (subst (fn a (x) (x y)) [y ↦ (x x)]))
   (term (fn b (z) (z (x x))))
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
  (test-equal (term (α=? (fn (a) (a x))
                         (fn (b) (b x))))
              #t)
  (test-equal (term (α=? (fn x (a) (a x))
                         (fn x (b) (b x))))
              #t)
  
  ;; α=? app tests
  (test-equal (term (α=? ((fn (a) (a x))
                          (fn (b) (b x)))
                         ((fn (c) (c x))
                          (fn (d) (d x)))))
              #t)
  )

(define (same-length? l1 l2)
  (equal? (length l1)
          (length l2)))

(define (top-level-hole? h)
  (equal? (term hole) h))

(define (arg-mismatch-msg xs args f)
  (list (string-append
         "Expected " (~a (length xs))
         " arguments, but found "
         (~a (length args)))
        (string-append
         "Form: " (~a (list* f
                             args)))))

;; -->v
;; Clojure reduction relation
(define -->v
  (reduction-relation
   Clojure
   #:domain M
   [--> (in-hole C ((fn (X ...) M) VA ...))
        (in-hole C (subst* M ([X ↦ VA] ...)))
        (judgment-holds (unique (X ...)))
        (side-condition (same-length? (term (X ...)) (term (VA ...))))
        β]
   [--> (in-hole C ((fn X_rec [X ...] M) VA ...))
        (in-hole C (subst* M ([X_rec ↦ (fn X_rec [X ...] M)]
                              [X ↦ VA] ...)))
        (judgment-holds (unique (X_rec X ...)))
        (side-condition (same-length? (term (X ...)) (term (VA ...))))
        rec-β]
   
   [--> (in-hole C (if VA M_1 M_2))
        (in-hole C M_1)
        (side-condition (truthy? (term VA)))
        if-t]
   [--> (in-hole C (if VA M_1 M_2))
        (in-hole C M_2)
        (side-condition (not (truthy? (term VA))))
        if-f]
   [--> (in-hole C (O VA ...))
        (in-hole C V_1)
        (judgment-holds (δ (O VA ...) V_1))
        δ]
   [--> (in-hole C ERR)
        ERR
        ;; prevent infinite top expansions of top-level errors
        (side-condition (not (top-level-hole? (term C))))
        error]
   [--> (in-hole C X)
        (error unknown-variable X)
        x-error]
   [--> (NONFNV VA ...) (error bad-application)
        β-non-function]
   [--> (in-hole C ((fn [X ...] M) VA ...))
        (error argument-mismatch
               ,(arg-mismatch-msg (term (X ...)) (term (VA ...)) (term (fn [X ...] M))))
        (judgment-holds (unique (X ...)))
        (side-condition (not (same-length? (term (X ...)) (term (VA ...)))))
        β-mismatch]
   [--> (in-hole C ((name f (fn X_f [X ...] M)) VA ...))
        (error argument-mismatch
               ,(arg-mismatch-msg (term (X ...)) (term (VA ...)) (term (fn [X ...] M))))
        (side-condition (not (same-length? (term (X ...)) (term (VA ...)))))
        rec-β-mismatch]
   ))

(module+ test
  ;; test for lambda (no reduction)
  (test--> -->v
           #:equiv α-equal?
           (term (fn (x) x)))
  ;; β
  (test--> -->v
           #:equiv α-equal?
           (term ((fn (x) x) 1))
           (term 1))
  ;; β in a context
  (test--> -->v
           #:equiv α-equal?
           (term (((fn (x) x) (fn (x) x)) 1))
           (term ((fn (x) x) 1))))


(define-syntax-rule (clj/def defname name args body)
  (define-term defname
    (fn name args body)))

(clj/def def/fact
         fact [n]
         (if (zero? n)
             1
             (* n (fact (dec n)))))

(define-term fact-5
  ((fn fact [n]
       (if (zero? n)
           1
           (* n (fact (dec n)))))
   5))

(define-extended-language ClojureSpec Clojure
  (FS ::= (DefFSpec (SP ...) SP))
  ;TODO blame paths
  (SP ::= P?)
  (C ::= .... (assert-spec C SP))
  (CNST ::= .... (gen-spec SP))
  (M ::= .... (assert-spec M SP)))

(define ngenerations 10)
(define min-random-int -2000)
(define max-random-int 2000)

(define (random-int)
  (random min-random-int max-random-int))

(define-metafunction ClojureSpec
  gen-spec* : SP -> V
  [(gen-spec* number?) ,(random-int)]
  [(gen-spec* zero?) 0]
  [(gen-spec* boolean?) ,(random-ref '(true false))]
  [(gen-spec* nil?) nil])

(define (spec-violation-msg p v)
  (string-append
   "Spec violation: "
   "Expected spec " (~a p)
   ", actual value " (~a v)))

(define -->vspec
  (extend-reduction-relation
    -->v ClojureSpec
    #:domain M

    ;; GenSpec
    (--> (in-hole C (gen-spec SP)) (in-hole C (gen-spec* SP))
         gen-spec)
   
    ;; AssertSpec
    (--> (in-hole C (assert-spec (fn [X ...] M)
                                 (DefFSpec (SP_a ...) SP_r)))
         (in-hole C (fn [X ...]
                        (assert-spec
                         ((fn [X ...] M)
                          (assert-spec X SP_a) ...)
                         SP_r)))
         assert-deffspec)
    ;; Treat recursive functions checked with DefFSpec
    ;; as top level mutable vars, so we spec recursive calls
    (--> (in-hole C (assert-spec (fn X_n [X ...] M)
                                 (DefFSpec (SP_a ...) SP_r)))
         (in-hole C (fn X_n [X ...]
                        (assert-spec
                         ((fn [X ...] M)
                          (assert-spec X SP_a) ...)
                         SP_r)))
         assert-rec-deffspec)

    (--> (in-hole C (assert-spec VA P?))
         (in-hole C (if (P? VA)
                        VA
                        (error spec-error
                               ,(spec-violation-msg (term P?) (term VA)))))
         assert-spec-P?)))

(define-extended-language ClojureSpecHOF ClojureSpec
  (SP ::= .... (FSpec (SP ...) SP) (FSpec (SP ...) SP NT)))

(define (dummy-params terms)
  (map (lambda (i) (string->symbol
                    (string-append
                     "x"
                     (number->string i))))
       (range (length terms))))

(define-metafunction/extension gen-spec* ClojureSpecHOF
  gen-spec*-hof : SP -> V
  [(gen-spec*-hof (FSpec (SP_a ...) SP_r))
   
       ;; TODO check arg specs
   (fn ,(dummy-params (term (SP_a ...))) (gen-spec*-hof SP_r))])

(define (nonf-spec-error-msg v)
  (string-append
   "Spec expected a function but found "
   (~a v)))

(define (do e1 e2)
  (term ((fn [,(gensym 'do)]
             ,e2)
         ,e1)))

(define -->vspec-hof
  (extend-reduction-relation
    -->vspec ClojureSpecHOF
    #:domain M
    
    ;; GenSpec
    (--> (in-hole C (gen-spec SP))
         (in-hole C (gen-spec*-hof SP))
         gen-spec)
   ; prepare FSpec gen-count
   (--> (in-hole C (assert-spec VA (FSpec (SP_a ...) SP_r)))
        (in-hole C (assert-spec VA (FSpec (SP_a ...) SP_r ,ngenerations)))
        assert-fspec-init)

   (--> (in-hole C (assert-spec (name f (fn [X ...] M)) (FSpec (SP_a ...) SP_r 0)))
        (in-hole C f)
        assert-fspec-stop)
 
   (--> (in-hole C (assert-spec (name f (fn [X ...] M)) (FSpec (SP_a ...) SP_r NT)))
        (in-hole C ,(do (term (assert-spec (f (gen-spec SP_a) ...) SP_r))
                        (term (assert-spec f (FSpec (SP_a ...) SP_r ,(sub1 (term NT)))))))
        (side-condition (< 0 (term NT)))
        assert-fspec-gen)

   (--> (in-hole C (assert-spec (name f (fn nme [X ...] M)) (FSpec (SP_a ...) SP_r NT)))
        (in-hole C ,(do (term (assert-spec (f (gen-spec SP_a) ...) SP_r))
                        (term (assert-spec f (FSpec (SP_a ...) SP_r ,(sub1 (term NT)))))))
        (side-condition (< 0 (term NT)))
        assert-rec-fspec-gen)

   (--> (in-hole C (assert-spec NONFNV
                                (FSpec (SP_a ...) SP_r NT)))
        (error spec-error
               ,(nonf-spec-error-msg (term NONFNV)))
        assert-fspec-nonf)))

(define-syntax-rule (eval-clj t)
  (apply-reduction-relation* -->v (term t)))
(define-syntax-rule (eval-clj-no-inject t)
  (apply-reduction-relation* -->v (term t)))
(define-syntax-rule (eval-clj-traces t)
  (traces -->v (term t)))

(define-syntax-rule (eval-cljspec t)
  (apply-reduction-relation* -->vspec (term t)))

(define-syntax-rule (eval-cljspec-no-inject t)
  (apply-reduction-relation* -->vspec (term t)))

(define-syntax-rule (eval-cljspec-traces t)
  (traces -->vspec (term t)))

(define-syntax-rule (eval-cljspec-hof t)
  (apply-reduction-relation* -->vspec-hof (term t)))
(define-syntax-rule (eval-cljspec-hof-no-inject t)
  (apply-reduction-relation* -->vspec-hof (term t)))
(define-syntax-rule (eval-cljspec-hof-traces t)
  (traces -->vspec-hof (term t)))

(define (singleton-pred p l)
  (and (not (empty? l))
       (empty? (cdr l))
       (p (car l))))

(define (singleton-error? p l)
  (singleton-pred (lambda (x)
                    (and (list? x)
                         (<= 2 (length x))
                         (equal? 'error (first x))
                         (p (second x))))
                  l))

(define (singleton-spec-error? l)
  (singleton-error? (lambda (x) (equal? 'spec-error x))
                    l))
(define (singleton-arg-mismatch-error? l)
  (singleton-error? (lambda (x) (equal? 'argument-mismatch x))
                    l))
(define (singleton-unknown-var-error? l)
  (singleton-error? (lambda (x) (equal? 'unknown-variable x))
                    l))

(test-equal (redex-match? Clojure M (term fact-5)) #t)
#| ;; FIXME uncomment
(test-equal (redex-match? ClojureSpecHOF C
                          (term (assert-spec
                                 ((y (gen-spec number?))
                                  (rho (y ((fn (x) x) (rho)))))
                                 zero?)))
            #t)
(test-equal (redex-match? ClojureSpecHOF S
                          (term ((y (gen-spec number?))
                                  (rho (y ((fn (x) x) (rho)))))))
            #t)
(test-equal (redex-match? ClojureSpecHOF ((assert-spec S SP) (frames F ...))
                          (term ((assert-spec
                                  ((y (gen-spec number?)) (rho (y ((fn (x) x) (rho)))))
                                  zero?)
                                 (frames
                                  (((fn
                                     (do3460218)
                                     (assert-spec y (FSpec (number?) zero? 9)))
                                    (rho (y ((fn (x) x) (rho)))))
                                   ())))))
            #t)
|#


(test-equal (eval-clj true)
            '(true))
(test-equal (eval-clj (if (zero? 0) 0 1))
            '(0))
(test-equal (eval-clj (if (zero? 1) 0 1))
            '(1))
(test-equal (eval-clj ((fn [x] 5) 1))
            '(5))
(test-equal (eval-clj fact-5)
            '(120))
(test-equal (eval-clj (inc 1))
            '(2))
(test-equal (eval-clj (HashMap))
            '((HashMap)))
(test-equal (eval-clj (assoc (HashMap) 0 1))
            '((HashMap (0 1))))
(test-equal (eval-clj (get (assoc (HashMap) 0 1) 0 nil))
            '(1))
(test-equal (eval-clj (get (assoc (HashMap) 0 1) 1 nil))
            '(nil))
(test-equal (eval-clj (get (dissoc (assoc (HashMap) 0 1) 0) 0 nil))
            '(nil))

(test-equal (eval-clj (def/fact 5))
            '(120))
(test-equal (eval-clj (error a))
            '((error a)))
(test-equal (eval-clj (if (error a) 0 1))
            '((error a)))
(test-equal (eval-clj (if 0 (error a) (error b)))
            '((error a)))
(test-equal (eval-clj (if nil (error a) (error b)))
            '((error b)))
(test-equal (eval-clj ((fn [x] (error a)) 1))
            '((error a)))
(test-equal (eval-clj ((fn [x] (error a)) (error b)))
            '((error b)))
(test-equal (eval-clj ((fn nme [x] (error a)) 1))
            '((error a)))
(test-equal (eval-clj ((fn nme [x] (error a)) (error b)))
            '((error b)))
(test-equal (eval-clj ((fn [x y] (error a))
                       (error c)
                       (error b)))
            '((error c)))
(test-equal (eval-clj ((fn nme [x y] (error a)) (error c) (error b)))
            '((error c)))
(test-equal (eval-clj (zero? (error a)))
            '((error a)))
(test-equal (eval-clj (assoc 1 2 (error a)))
            '((error a)))
(test-equal (eval-clj (+ 2 (error a)))
            '((error a)))
(test-equal (singleton-arg-mismatch-error?
             (eval-clj ((fn [] 1) 1)))
            #t)
(test-equal (singleton-arg-mismatch-error?
             (eval-clj ((fn nme [] 1) 1)))
            #t)
(test-equal (singleton-arg-mismatch-error?
             (eval-clj ((fn [x] 1))))
            #t)
(test-equal (singleton-arg-mismatch-error?
             (eval-clj ((fn nme [x] 1))))
            #t)

(test-equal (singleton-unknown-var-error?
             (eval-clj x))
            #t)

(test-equal (eval-clj (1 2))
            '((error bad-application)))

;;spec tests
(test-equal (eval-cljspec 2)
            '(2))
(test-equal (eval-cljspec (+ 2 2))
            '(4))
(test-equal (eval-cljspec (+ 2 (error a)))
            '((error a)))
(test-equal (singleton-spec-error? (eval-cljspec (assert-spec 1 zero?)))
            #t)
(test-equal (eval-cljspec (assert-spec 0 zero?))
            '(0))
(test-equal (eval-cljspec (assert-spec 0 number?))
            '(0))
(test-equal (singleton-spec-error? (eval-cljspec (assert-spec true number?)))
            #t)
(test-equal (eval-cljspec (assert-spec true boolean?))
            '(true))
(test-equal (eval-cljspec (assert-spec false boolean?))
            '(false))
(test-equal (singleton-spec-error? (eval-cljspec (assert-spec 1 boolean?)))
            #t)


;spec-hof tests
(test-equal (singleton-pred number? (eval-cljspec-hof (gen-spec number?)))
            #t)
(test-equal (singleton-pred (lambda (x) (or (equal? 'true x)
                                            (equal? 'false x)))
                            (eval-cljspec-hof (gen-spec boolean?)))
            #t)
(test-equal (singleton-pred (lambda (x) (and (number? x)
                                             (zero? x)))
                            (eval-cljspec-hof (gen-spec zero?)))
            #t)

(test-equal (singleton-pred number?
                            (eval-cljspec-hof ((gen-spec (FSpec () number?)))))
            #t)
(test-equal (singleton-pred number?
                            (eval-cljspec-hof ((gen-spec (FSpec (number?) number?)) 1)))
            #t)
;; TODO check arguments of generated fn's
#;(test-equal (singleton-spec-error? (eval-cljspec-hof ((gen-spec (FSpec (number?) number?)) nil)))
               #t)

(test-equal (eval-cljspec-hof (assert-spec (fn [x] x) (FSpec (zero?) zero?)))
            '((fn [x] x)))
(test-equal (singleton-spec-error?
             (eval-cljspec-hof (assert-spec (fn [x] x) (FSpec (number?) zero?))))
            #t)
(test-equal (eval-cljspec-hof (assert-spec (fn [x] x) (FSpec (zero?) number?)))
            '((fn [x] x)))

(test-equal (eval-cljspec-no-inject (assert-spec
                                      1
                                      number?))
            '(1))




; ifn? test for functions
(test-equal (singleton-spec-error? (eval-cljspec-hof (assert-spec 1 (FSpec () number?))))
            #t)
; mismatch between FSpec arg count and actual count
;; removed `assert-fspec-gen-arg-mismatch`, so not a relevant test
#;(test-equal (singleton-spec-error? (eval-cljspec-hof (assert-spec (fn [x] x) (FSpec () number?))))
            #t)
(test-equal (singleton-arg-mismatch-error? (eval-cljspec-hof (assert-spec (fn [x] x) (FSpec () number?))))
            #t)


(define-syntax-rule (check-compatible-result speclang orig-clj orig-cljspec eclj ecljspec)
  (...
   (begin
    (unless (redex-match? Clojure V eclj)
      (error "Clojure evaluation did not fully reduce"
             'original-form
             orig-clj
             'stuck-form
             eclj))
    (unless (redex-match? speclang V ecljspec)
      (error (string-append
              "ClojureSpec evaluation did not fully reduce\n"
              "Original-form: " (~a orig-cljspec) "\n"
              
              "Stuck-form: " (~a ecljspec))))
    (cond
      ;; exactly the same output
      [(equal? eclj ecljspec) #t]
      ;; throws a spec error
      [(redex-match? speclang (error spec-error any ...) ecljspec) #t]
      ;; throws a different kind of error than Clojure
      [(and (redex-match? Clojure ERR eclj)
            (redex-match? speclang ERR ecljspec)) #t]
      ;; spec throws a non-spec error where Clojure returns a value
      [else (error (string-append
                    "ClojureSpec evaluation returned a different value than Clojure\n"
                    "Original-clj: " (~a orig-clj)
                   "\nOriginal-clj-spec: " (~a orig-cljspec)
                   "\nclj-value: " (~a eclj)
                   "\ncljspec-value: " (~a ecljspec)))]))))

;; check that errors are actually caught and thrown
(check-error
 (check-compatible-result ClojureSpecHOF
                          (term (fn () 1))
                          (term (assert-spec (fn () 1) (FSpec (number?) zero?)))
                          '(fn () 1)
                          '(error argument-mismatch ("Expected 0 arguments, but found 1" "Form: ((fn () 1) 1771)"))))



(define-syntax-rule (check-Clojure-ClojureSpec-compat* speclang eval-cljspec nforms nspecs)
  (begin
    (for/list ([i (in-range nforms)]
               [j (in-range nspecs)])
      (let* ([ct (generate-term Clojure M #:i-th (* 200 i))]
             [sp (generate-term speclang SP #:i-th (+ i j))]
             [orig-clj ct]
             [orig-cljspec (term (assert-spec ,ct ,sp))]
             [esclj (eval-clj ,orig-clj)]
             [escljspec (eval-cljspec ,orig-cljspec)]
             [eclj (if (equal? 1 (length esclj))
                       (first esclj)
                       (error "Multiple results from Clojure eval"
                              esclj))]
             [ecljspec (if (equal? 1 (length escljspec))
                           (first escljspec)
                           (error "Multiple results from ClojureSpec eval"
                                  escljspec))])
        (check-compatible-result
         speclang
         orig-clj
         orig-cljspec
         eclj
         ecljspec)))
    #f))

(define (check-Clojure-ClojureSpec-compat nforms nspecs)
  (check-Clojure-ClojureSpec-compat* ClojureSpec eval-cljspec nforms nspecs))

#; (check-Clojure-ClojureSpec-compat 1000 1000)
#; (check-Clojure-ClojureSpecHOF-compat 1000 1000)
(define (check-Clojure-ClojureSpecHOF-compat nforms nspecs)
  (check-Clojure-ClojureSpec-compat* ClojureSpecHOF eval-cljspec-hof nforms nspecs))

; the counter-example
(test-equal (eval-cljspec-hof (assert-spec (fn [x] (if (zero? x) nil (error missiles-launched)))
                                           (FSpec (number?) nil?)))
            '((error missiles-launched)))

(test-equal (eval-clj (fn [x] (if (zero? x) nil (error missiles-launched))))
            '((fn [x] (if (zero? x) nil (error missiles-launched)))))

(define-syntax-rule (rlang . args)
  (with-rewriters
      (lambda () (render-language . args))))

(define-syntax-rule (rrr . args)
  (with-rewriters
      (lambda () (render-reduction-relation . args))))
(define-syntax-rule (rmf . args)
  (with-rewriters
      (lambda () (render-metafunction . args))))

#;
(begin
  (rlang Clojure "clojure-grammar.pdf")
  (rlang ClojureSpec "clojurespec-grammar.pdf")
  (rlang ClojureSpecHOF "clojurespechof-grammar.pdf")
  
  (rrr -->v "arrowv.pdf")
  (rrr -->vspec "arrowvspec.pdf")
  (rrr -->vspec-hof "arrowvspec-hof.pdf")
  (rmf gen-spec* "gen-spec*.pdf" #:contract? true)
  (rmf gen-spec*-hof "gen-spec*-hof.pdf" #:contract? true)
  )
