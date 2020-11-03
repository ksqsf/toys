#lang racket

(require (for-syntax syntax/parse))

;; closure
(struct CLOS (env var body) #:transparent)

(define (extend rho x v)
  (cons (cons x v) rho))

(define (val0 rho e)
  (match e
    [`(λ (,x) ,b)
     (CLOS rho x b)]
    [x #:when (symbol? x)
       (let ((xv (assv x rho)))
         (if xv
             (cdr xv)
             (error 'val "Unknown variable ~a" x)))]
    [`(,rator ,rand)
     (do-ap0 (val0 rho rator) (val0 rho rand))]))

(define (do-ap0 clos arg)
  (match clos
    [(CLOS rho x b)
     (val0 (extend rho x arg) b)]))

(define (run-program0 rho exprs)
  (match exprs
    ['() (void)]
    [(cons `(define ,x ,e) rest)
     (let ([v (val0 rho e)])
       (run-program0 (extend rho x v) rest))]
    [(cons e rest)
     (displayln (val0 rho e))
     (run-program0 rho rest)]))

(run-program0 '()
             '((define id (λ (x) x))
               (id (λ (y) (λ (z) (z y))))))


(define (add-* x)
  (string->symbol
   (string-append (symbol->string x)
                  "*")))

(define (freshen used x)
  (if (memv x used)
      (freshen used (add-* x))
      x))

;
; Normalization = Evaluation + Reading back
;

; Introduce neutral variables & applications
(struct N-var (name))
(struct N-ap (rator rand))

; app w/ neutral
(define (do-ap fun arg)
  (match fun
    [(CLOS rho x b)
     (val (extend rho x arg) b)]
    [neutral-fun
     (N-ap fun arg)]))

; eval w/ neutral
(define (val ρ e)
  (match e
    [`(λ (,x) ,b)
     (CLOS ρ x b)]
    [x #:when (symbol? x)
     (let ((xv (assv x ρ)))
       (if xv
           (cdr xv)
           (error 'val "Unknown variable ~v" x)))]
    [`(,rator ,rand)
     (do-ap (val ρ rator) (val ρ rand))]))

(define (read-back used-names v)
  (match v
    [(CLOS rho x body)
     (let* ((y (freshen used-names x))
            (neutral-y (N-var y)))
       `(λ (,y)
          ,(read-back (cons y used-names)
                      (val (extend rho x neutral-y) body))))]
    [(N-var x) x]
    [(N-ap rator rand)
     `(,(read-back used-names rator) ,(read-back used-names rand))]))

(read-back '() (val '() '((λ (x) (λ (y) (x y))) (λ (x) x))))

(define (norm rho e)
  (read-back '() (val rho e)))

(define (run-program rho exprs)
  (match exprs
    [(list) (void)]
    [(list `(define ,x ,e) rest ...)
     (let ([v (val rho e)])
       (run-program (extend rho x v) rest))]
    [(list e rest ...)
     (displayln (norm rho e))
     (run-program rho rest)]))

(define (with-numerals e)
  `((define church-zero
      (λ (f)
        (λ (x)
          x)))
    (define church-add1
      (λ (n-1)
        (λ (f)
          (λ (x)
            (f ((n-1 f) x))))))
    ,e))

(define (to-church n)
  (cond [(zero? n) 'church-zero]
        [(positive? n)
         (let ([church-of-n-1 (to-church (sub1 n))])
           `(church-add1 ,church-of-n-1))]))

(run-program '() (with-numerals (to-church 0)))
(run-program '() (with-numerals (to-church 1)))
(run-program '() (with-numerals (to-church 4)))

(define church-add
  `(λ (j)
     (λ (k)
       (λ (f)
         (λ (x)
           ((j f) ((k f) x)))))))

(run-program '() (with-numerals `((,church-add ,(to-church 2)) ,(to-church 2))))

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

(define (bigger-than-two n)
  (if (> n 2)
      (go n)
      (stop n "Not greater than two")))

(define (type=? t1 t2)
  (match* (t1 t2)
    [('Nat 'Nat) #t]
    [(`(-> ,A1 ,B1) `(-> ,A2 ,B2))
     (and (type=? A1 A2) (type=? B1 B2))]
    [(_ _) #f]))

(define (type? t)
  (type=? t t))

(type? 'Nat)
(type? '(-> Nat Nat))

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
             [_ (check Γ step `(-> Nat (-> ,type ,type)))])
            (go type))]
    [x #:when (and (symbol? x)
                   (not (memv x '(the rec λ zero add1))))
       (match (assv x Γ)
         [#f (stop x "Variable not found")]
         [(cons _ t) (go t)])]
    [`(,rator ,rand)
     (go-on ([rator-t (synth Γ rator)])
            (match rator-t
              [`(-> ,A ,B)
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
       [`(-> ,A, B)
        (go-on ([_ (check (extend Γ x A) b B)])
               (go 'ok))]
       [non-arrow
        (stop e (format "Instead of -> type, got ~a" non-arrow))])]
    [other
     (go-on ([t2 (synth Γ e)])
            (if (type=? t t2)
                (go 'ok)
                (stop e
                      (format "Synthesized type ~v where ~v was expected"
                              t2 t))))]))

(check '() '(λ (x) x) '(-> Nat Nat))
(check '() 'zero 'Nat)

(define (check-program Γ prog)
  (match prog
    ['()
     (go Γ)]
    [(cons `(define ,x ,e) rest)
     (go-on ([t (synth Γ e)])
       (check-program (extend Γ x t) rest))]
    [(cons e rest)
     (go-on ([t (synth Γ e)])
       (begin
         (printf "~a has type ~a\n" e t)
         (check-program Γ rest)))]))

(check-program '()
               '((define three
                   (the Nat
                        (add1 (add1 (add1 zero)))))
                 (define +
                   (the (-> Nat (-> Nat Nat))
                        (λ (n)
                          (λ (k)
                            (rec Nat n
                              k
                              (λ (pred)
                                (λ (almost-sum)
                                  (add1 almost-sum))))))))
                 (+ three)
                 ((+ three) three)))


