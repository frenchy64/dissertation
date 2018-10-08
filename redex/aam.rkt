#lang racket

(require redex)
(require redex-aam-tutorial/subst)

(define-language PCF
  (M ::=
     N O X L
     (μ (X : T) L)
     (M M ...)
     (if0 M M M))
  (X ::= variable-not-otherwise-mentioned)
  (L ::= (λ ([X : T] ...) M))
  (V ::= N O L)
  (N ::= number)
  (O ::= O1 O2)
  (O1 ::= add1 sub1)
  (O2 ::= + *)
  (T ::= num (T ... -> T)))

(define-term fact-5
  ((μ (fact : (num -> num))
      (λ ([n : num])
        (if0 n
             1
             (* n (fact (sub1 n))))))
   5))

(test-equal (redex-match? PCF M (term fact-5)) #t)

(define-extended-language PCFT PCF
  (Γ ::= (gamma (X T) ...)))

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


(define-judgment-form PCFT
    #:mode (⊢ I I I O)
    #:contract (⊢ Γ M : T)
    [(lookup Γ X T)
     -------------- var
     (⊢ Γ X : T)]
    [------------- num
     (⊢ Γ N : num)]
    [----------------------- op1
     (⊢ Γ O1 : (num -> num))]
    [--------------------------- op2
     (⊢ Γ O2 : (num num -> num))]
    [(⊢ Γ M_1 : num)
     (⊢ Γ M_2 : T)
     (⊢ Γ M_3 : T)
     --------------------------- if0
     (⊢ Γ (if0 M_1 M_2 M_3) : T)]
    [(⊢ (ext Γ (X T)) L : T)
     ----------------------- μ
     (⊢ Γ (μ (X : T) L) : T)]
    [(⊢ Γ M_0 : (T_1 ..._1 -> T))
     (⊢ Γ M_1 : T_1) ...
     ----------------------- app
     (⊢ Γ (M_0 M_1 ..._1) : T)]
    [(unique (X ...))
     (⊢ (ext Γ (X T) ...) M : T_n)
     ------------------------------------------ λ
     (⊢ Γ (λ ([X : T] ...) M) : (T ... -> T_n))])

(define-judgment-form PCF
  #:mode (δ I O)
  #:contract (δ (O N ...) N)
  [(δ (+ N_0 N_1) ,(+ (term N_0) (term N_1)))]
  [(δ (* N_0 N_1) ,(* (term N_0) (term N_1)))]
  [(δ (sub1 N) ,(sub1 (term N)))]
  [(δ (add1 N) ,(add1 (term N)))])

(define r
  (reduction-relation
   PCF #:domain M
   (--> (μ (X : T) M)
        (subst (X (μ (X : T) M)) M)
        μ)
  
   (--> ((λ ([X : T] ...) M_0) M ...)
        (subst (X M) ... M_0)
        β)
  
   (--> (O N_0 ...) N_1
        (judgment-holds (δ (O N_0 ...) N_1))
        δ)
  
   (--> (if0 0 M_1 M_2) M_1
        if-t)
   (--> (if0 N M_1 M_2) M_2
        (side-condition (not (zero? (term N))))
        if-f)))

(define -->r (compatible-closure r PCF M))

(define-extended-language PCFn PCF
  (E ::= hole
     (E M ...)
     (O V ... E M ...)
     (if0 E M M)))
(define-extended-language PCFv PCF
  (E ::= hole
     (V ... E M ...)
     (if0 E M M)))

(define -->n
  (context-closure r PCFn E))

(define v
  (extend-reduction-relation
   r PCF #:domain M
   (--> ((λ ([X : T] ...) M_0) V ...)
        (subst (X V) ... M_0)
        β)))

(define -->v
  (context-closure v PCFv E))

(define-extended-language PCF⇓ PCF
  (V ::= N O (L ρ) ((μ (X : T) L) ρ))
  (ρ ::= (rho (X V) ...)))

(define-judgment-form PCF⇓
  #:mode (⇓ I I I O)
  #:contract (⇓ M ρ : V)
  
  [(⇓ N ρ : N)]
  [(⇓ O ρ : O)]
  [(⇓ L ρ : (L ρ))]
  [(⇓ (μ (X_f : T_f) L) ρ : ((μ (X_f : T_f) L) ρ))]
  
  [(lookup ρ X V)
   --------------
   (⇓ X ρ : V)]
  
  [(⇓ M_0 ρ : N)
   (where M ,(if (zero? (term N)) (term M_1) (term M_2)))
   (⇓ M ρ : V)
   ---------------------------
   (⇓ (if0 M_0 M_1 M_2) ρ : V)]
  
  [(⇓ M_0 ρ : O)
   (⇓ M_1 ρ : N)
   ...
   (δ (O N ...) N_1)
   -----------------------
   (⇓ (M_0 M_1 ...) ρ : N_1)]
  
  [(⇓ M_0 ρ : ((λ ([X_1 : T] ...) M) ρ_1))
   (⇓ M_1 ρ : V_1)
   ...
   (⇓ M (ext ρ_1 (X_1 V_1) ...) : V)
   -----------------------------------
   (⇓ (M_0 M_1 ...) ρ : V)]
  
  [(⇓ M_0 ρ : (name f ((μ (X_f : T_f) (λ ([X_1 : T] ...) M)) ρ_1)))
   (⇓ M_1 ρ : V_1)
   ...
   (⇓ M (ext ρ_1 (X_f f) (X_1 V_1) ...) : V)
   -----------------------------------------
   (⇓ (M_0 M_1 ...) ρ : V)])

(define-extended-language PCFρ PCF⇓
  (C ::= V (M ρ) (if0 C C C) (C C ...))
  (E ::= hole (V ... E C ...) (if0 E C C)))

(define vρ
  (reduction-relation
   PCFρ #:domain C
   (--> ((if0 M ...) ρ) (if0 (M ρ) ...) ρ-if)
   (--> ((M ...) ρ) ((M ρ) ...) ρ-app)
   (--> (O ρ) O ρ-op)
   (--> (N ρ) N ρ-num)
   (--> (X ρ) V
        (judgment-holds (lookup ρ X V))
        ρ-x)
  
   (--> (((λ ([X : T] ...) M) ρ) V ...)
        (M (ext ρ (X V) ...))
        β)
  
   (--> ((name f ((μ (X_f : T_f) (λ ([X : T] ...) M)) ρ)) V ...)
        (M (ext ρ (X_f f) (X V) ...))
        rec-β)
  
   (--> (O V ...) V_1
        (judgment-holds (δ (O V ...) V_1))
        δ)
  
   (--> (if0 0 C_1 C_2) C_1 if-t)
   (--> (if0 N C_1 C_2) C_2
        (side-condition (not (equal? 0 (term N))))
        if-f)))


(define -->vρ
  (context-closure vρ PCFρ E))

(define-metafunction PCFρ
  injρ : M -> C
  [(injρ M) (M (rho))])

(define-extended-language PCFς PCFρ
  (F ::= (V ... [] C ...) (if0 [] C C))
  (K ::= (continuations F ...))
  (S ::= ; serious terms S ∩ V = ∅, C = S ∪ V
     (N ρ)
     (O ρ)
     (X ρ)
     ((M M ...) ρ)
     ((if0 M M M) ρ)
     (if0 C C C)
     (C C ...))
  (ς ::= (C K) V))

(define -->vς
  (extend-reduction-relation
   ; Apply
   (context-closure vρ PCFς (hole K))
   PCFς
   ; Eval
   (--> ((if0 S_0 C_1 C_2) (continuations F ...))
        (S_0 (continuations (if0 [] C_1 C_2) F ...))
        ev-if)
  
   (--> ((V ... S C ...) (continuations F ...))
        (S (continuations (V ... [] C ...) F ...))
        ev-app)
  
   ; Continue
   (--> (V (continuations)) V halt)
  
   (--> (V (continuations (if0 [] C_1 C_2) F ...))
        ((if0 V C_1 C_2) (continuations F ...))
        co-if)
  
   (--> (V (continuations (V_0 ... [] C_0 ...) F ...))
        ((V_0 ... V C_0 ...) (continuations F ...))
        co-app)))

(define-metafunction PCFς
  injς : M -> ς
  [(injς M) ((injρ M) (continuations))])


(define-extended-language PCFσ PCFς
  (ρ ::= (rho (X A) ...))
  (Σ ::= (heap (A V) ...))
  (A ::= any)
  (σ ::= (ς Σ) V))

(define-metafunction PCFσ
  injσ : M -> σ
  [(injσ M) ((injς M) (heap))])

(define-metafunction PCFσ
  formals : M -> (X ...)
  [(formals (λ ([X : T] ...) M)) (X ...)]
  [(formals (μ (X_f : T_f) L)) (X_f X ...)
                               (where (X ...) (formals L))])

(define-syntax-rule (-->vσ/alloc alloc)
  (...
   (extend-reduction-relation
    (context-closure -->vς PCFσ (hole Σ))
    PCFσ
    (--> (N Σ) N discard-Σ-N)
    (--> (O Σ) O discard-Σ-O)
    (--> (((X ρ) K) Σ) ((V K) Σ)
         (judgment-holds (lookup ρ X A))
         (judgment-holds (lookup Σ A V))
         ρ-x)
  
    (--> (name σ (((((λ ([X : T] ...) M) ρ) V ...) K) Σ))
         (((M (ext ρ (X A) ...)) K) (ext Σ (A V) ...))
         (where (A ...) (alloc σ))
         β)
  
    (--> (name σ ((((name f ((μ (X_f : T_f) (λ ([X : T] ...) M)) ρ)) V ...) K) Σ))
         (((M (ext ρ (X_f A_f) (X A) ...)) K) (ext Σ (A_f f) (A V) ...))
         (where (A_f A ...) (alloc σ))
           rec-β))))

(define-metafunction PCFσ
  alloc : ((C K) Σ) -> (A ...)
  [(alloc ((((M ρ) V ...) K) Σ))
   ,(map (λ (x) (list x (gensym x)))
         (term (formals M)))])

(define -->vσ (-->vσ/alloc alloc))

(define (-->σ-to--->ς t)
  (equal? (apply-reduction-relation* -->vσ (term (injσ ,t)))
          (apply-reduction-relation* -->vς (term (injς ,t)))))

(test-equal (-->σ-to--->ς (term fact-5)) #t)

(define-extended-language PCFσ* PCFσ
  (K ::= (continuations) (continuations F A))
  (Σ ::= (heap (A U) ...))
  (U ::= V K))

(define-metafunction/extension alloc PCFσ*
  alloc* : ((C K) Σ) -> (A ...)
  [(alloc* (((if0 S_0 C_1 C_2) K) Σ))
   (((if0 [] C_1 C_2) ,(gensym 'if0)))]
  [(alloc* (((V ... S C ...) K) Σ))
   (((V ... [] C ...) ,(gensym 'app)))])

(define-syntax-rule
  (-->vσ*/alloc alloc*)
  (...
   (extend-reduction-relation
    (-->vσ/alloc alloc*)
    PCFσ*
    ; Eval
    (--> (name σ (((if0 S_0 C_1 C_2) K) Σ))
         ((S_0 ((if0 [] C_1 C_2) A)) (ext Σ (A K)))
         (where (A) (alloc* σ))
         ev-if)
  
    (--> (name σ (((V ... S C ...) K) Σ))
         ((S ((V ... [] C ...) A)) (ext Σ (A K)))
         (where (A) (alloc* σ))
         ev-app)
  
    ; Continue
    (--> ((V ((if0 [] C_1 C_2) A)) Σ)
         (((if0 V C_1 C_2) K) Σ)
         (judgment-holds (lookup Σ A K))
         co-if)
  
    (--> ((V ((V_0 ... [] C_0 ...) A)) Σ)
         (((V_0 ... V C_0 ...) K) Σ)
         (judgment-holds (lookup Σ A K))
         co-app))))

(define -->vσ* (-->vσ*/alloc alloc*))

(define-syntax-rule (-->vσ/Σ alloc ext-Σ lookup-Σ)
  (...
   (extend-reduction-relation
    (context-closure -->vς PCFσ (hole Σ))
    PCFσ
    (--> (N Σ) N discard-Σ-N)
    (--> (O Σ) O discard-Σ-O)
    (--> (((X ρ) K) Σ) ((V K) Σ)
         (judgment-holds (lookup ρ X A))
         (judgment-holds (lookup-Σ Σ A V))
         ρ-x)
  
    (--> (name σ (((((λ ([X : T] ...) M) ρ) V ...) K) Σ))
         (((M (ext ρ (X A) ...)) K) (ext-Σ Σ (A V) ...))
         (where (A ...) (alloc σ))
         β)
  
    (--> (name σ ((((name f ((μ (X_f : T_f) (λ ([X : T] ...) M)) ρ)) V ...) K) Σ))
         (((M (ext ρ (X_f A_f) (X A) ...)) K) (ext-Σ Σ (A_f f) (A V) ...))
         (where (A_f A ...) (alloc σ))
         rec-β))))
