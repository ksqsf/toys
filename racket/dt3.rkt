#lang racket

(require (for-syntax syntax/parse))

(struct CLOS (env var body) #:transparent)
(define (extend rho x v)
  (cons (cons x v) rho))

(define (add-* x)
  (string->symbol
   (string-append (symbol->string x)
                  "*")))
(define (freshen used x)
  (if (memv x used)
      (freshen used (add-* x))
      x))

(struct go (result) #:transparent)
(struct stop (expr message) #:transparent)

(define-syntax (go-on stx)
  (syntax-parse stx
    [(go-on () result)
     (syntax/loc stx
       result)]
    [(go-on ([pat0 e0] [pat e] ...) result)
     (syntax/loc stx
       (match e0
         [(go pat0) (go-on ([pat e] ...) result)]
         [(go v) (error 'go-on "Patten did not match value ~v" v)]
         [(stop expr msg) (stop expr msg)]))]))

(define (type=? t1 t2)
  (match* (t1 t2)
    [('Nat 'Nat) #t]
    [(`(→ ,A1 ,B1) `(→ ,A2 ,B2))
     (and (type=? A1 A2) (type=? B1 B2))]
    [(_ _) #f]))

(define (type? t)
  (type=? t t))

(define (synth Γ e) ;; usually elimination
  (match e
    ; Type annotations
    [`(the ,t ,e2)
     (if (not (type? t))
         (stop e (format "Invalid type ~a" t))
         (go-on ([_ (check Γ e2 t)])
                (go t)))]
    ; Recursion on Nat
    [`(rec ,type ,target ,base ,step)
     (go-on ([target-t (synth Γ target)]
             [_ (if (type=? target-t 'Nat)
                    (go 'ok)
                    (stop target (format "Expected Nat, got ~v"
                                         target-t)))]
             [_ (check Γ base type)]
             [_ (check Γ step `(→ Nat (→ ,type ,type)))])
            (go type))]
    [x #:when (and (symbol? x)
                   (not (memv x '(the rec λ zero add1))))
       (match (assv x Γ)
         [#f (stop x "Variable not found")]
         [(cons _ t) (go t)])]
    [`(,rator ,rand)
     (go-on ([rator-t (synth Γ rator)])
            (match rator-t
              [`(→ ,A ,B)
               (go-on ([_ (check Γ rand A)])
                      (go B))]
              [else (stop rator (format "Not a function type: ~v"
                                        rator-t))]))]))

(define (check Γ e t) ;; usually introduction
  (match e
    ['zero
     (if (type=? t 'Nat)
         (go 'ok)
         (stop e (format "Tried to use ~v for zero" t)))]
    [`(add1 ,n)
     (if (type=? t 'Nat)
         (go-on ([_ (check Γ n 'Nat)])
                (go 'ok))
         (stop e (format "Tried to use ~v for add1" t)))]
    [`(λ (,x) ,b)
     (match t
       [`(→ ,A, B)
        (go-on ([_ (check (extend Γ x A) b B)])
               (go 'ok))]
       [non-arrow
        (stop e (format "Instead of → type, got ~a" non-arrow))])]
    [other
     (go-on ([t2 (synth Γ e)])
            (if (type=? t t2)
                (go 'ok)
                (stop e
                      (format "Synthesized type ~v where ~v was expected"
                              t2 t))))]))

(struct ZERO () #:transparent)
(struct ADD1 (pred) #:transparent)
(struct NEU (type neu) #:transparent) ; reflection: embedding of expr into val

(struct N-var (name) #:transparent)
(struct N-ap (rator rand) #:transparent)
(struct N-rec (type target base step) #:transparent)

(struct THE (type value) #:transparent)
(define (norm? v)
  (THE? v))

(define (val rho e)
  (match e
    [`(the ,type ,expr)
     (val rho expr)]
    ['zero (ZERO)]
    [`(add ,n) (ADD1 (val rho n))]
    [x #:when (and (symbol? x)
                   (not (memv '(the zero add1 λ rec))))
       (cdr (assv x rho))]
    [`(λ (,x) (,b))
     (CLOS rho x b)]
    [`(rec ,type ,target ,base ,step)
     (do-rec type (val rho target) (val rho base) (val rho step))]
    [`(,rator ,rand)
     (do-ap (val rho rator) (val rho rand))]))

(define (do-ap fun arg)
  (match fun
    [(CLOS rho x e)
     (val (extend rho x arg) e)]
    [(NEU `(→ ,A ,B) ne)
     (NEU B (N-ap ne (THE A arg)))]))

(define (do-rec type target base step)
  (match target
    [(ZERO) base]
    [(ADD1 n)
     (do-ap (do-ap step n)
            (do-rec type n base step))]
    [(NEU 'Nat ne)
     (NEU type
          (N-rec type
                 ne
                 (THE type base)
                 (THE `(→ Nat (→ ,type ,type)) step)))]))

(define (read-back used-names type value)
  (match type
    ['Nat
     (match value
       [(ZERO) 'zero]
       [(ADD1 n) `(add1 ,(read-back used-names 'Nat n))]
       [(NEU _ ne)
        (read-back-neutral used-names ne)])]
    [`(→ ,A ,B)
     (let ([x (freshen used-names 'x)])
       `(λ (,x)
          ,(read-back (cons x used-names)
                      B
                      (do-ap value (NEU A (N-var x))))))]))

(define (read-back-neutral used-names ne)
  (match ne
    [(N-var x) x]
    [(N-ap fun (THE arg-type arg))
     `(,(read-back-neutral used-names fun)
       ,(read-back used-names arg-type arg))]
    [(N-rec type target (THE base-type base) (THE step-type step))
     `(rec ,type
        ,(read-back-neutral used-names target)
        ,(read-back used-names base-type base)
        ,(read-back used-names step-type step))]))

(struct def (type value) #:transparent)
(define (defs->ctx Δ)
  (match Δ
    ['() '()]
    [(cons (cons x (def type _)) rest)
     (extend (defs->ctx rest) x type)]))
(define (defs->env Δ)
  (match Δ
    ['() '()]
    [(cons (cons x (def _ value)) rest)
     (extend (defs->env rest) x value)]))

(define (run-program Δ prog)
  (match prog
    ['() (go Δ)]
    [(cons `(define ,x ,e) rest)
     (go-on ([type (synth (defs->ctx Δ) e)])
            (run-program (extend Δ x (def type (val (defs->env Δ) e)))
                         rest))]
    [(cons e rest)
     (let ([Γ (defs->ctx Δ)]
           [ρ (defs->env Δ)])
       (go-on ([type (synth Γ e)])
              (let ([v (val ρ e)])
                (begin
                  (printf "(the ~a\n  ~a)\n"
                     type
                     (read-back (map car Γ) type v))
             (run-program Δ rest)))))]))
(run-program '() '((define +
                       (the (→ Nat
                                (→ Nat
                                    Nat))
                            (λ (x)
                              (λ (y)
                                (rec Nat x
                                  y
                                  (λ (_)
                                    (λ (sum)
                                      (add1 sum))))))))
                     +
                     (+ (add1 (add1 zero)))
                     ((+ (add1 (add1 zero))) (add1 zero))))