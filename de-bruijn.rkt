#lang racket

;;;
;;; Lambda calculus de Bruijn notation interpreter
;;; w/ bracket abstraction
;;; 

;;
;; Constructors
;;

(define (abs term) (cons 'λ term))
(define (app λ term) (cons λ term))

;;
;; Predicates
;;

(define (λ? exp)
  (and (pair? exp)
       (eq? (car exp) 'λ)))

(define (app? exp)
  (and (not (λ? exp))
       (pair? exp)))

(define (term? exp)
  (number? exp))

(define (closed?-iter exp depth)
  (cond
    [(term? exp)
     (< exp depth)]
    [(app? exp)
     (and (closed?-iter (car exp) depth)
          (closed?-iter (cdr exp) depth))]
    [(λ? exp)
     (closed?-iter (cdr exp) (+ depth 1))]))

(define (closed? exp)
  (closed?-iter exp 0))

;;
;; Lift free variables
;;

(define (lift-iter exp depth)
  (cond
    [(and (term? exp)
          (>= exp depth))
     (+ 1 exp)]
    [(term? exp)
     exp]
    [(app? exp)
     (app (lift-iter (car exp) depth)
          (lift-iter (cdr exp) depth))]
    [(λ? exp)
     (abs (lift-iter (cdr exp) (+ depth 1)))]))

(define (lift exp)
  (lift-iter exp 0))

(define (unlift-iter exp depth)
  (cond
    [(and (term? exp)
          (>= exp depth))
     (- exp 1)]
    [(term? exp)
     exp]
    [(app? exp)
     (app (unlift-iter (car exp) depth)
          (unlift-iter (cdr exp) depth))]
    [(λ? exp)
     (abs (unlift-iter (cdr exp) (+ depth 1)))]
    [#t
     exp]))

(define (unlift exp)
  (unlift-iter exp 0))

;;
;; Substitution
;;

(define (subst-iter e t d)
  (cond
    [(λ? e)
     (abs (subst-iter (cdr e) (lift t) (+ d 1)))]
    [(app? e)
     (app (subst-iter (car e) t d)
          (subst-iter (cdr e) t d))]
    [(= e (- d 1))
     t]
    [(< e (- d 1))
     e]
    [(>= e d)
     (- e 1)]))

;;
;; Application
;;

(define (apply e t)
  (subst-iter (cdr e) t 1))

;;
;; Reduction
;;

(define (reduce-step e)
  (cond
    [(term? e)
     e]
    [(and (app? e)
          (λ? (car e)))
     (apply (car e) (cdr e))]
    [(app? e)
     (app (reduce (car e))
          (reduce (cdr e)))]
    [(λ? e)
     (abs (reduce (cdr e)))]))

(define (reduce-iter e old)
  (if (equal? e old)
      e
      (reduce-iter (reduce-step e) e)))

(define (reduce e)
  (reduce-iter (reduce-step e) e))

;;
;; Combinatory logic example
;;

(define r reduce-step)
(define rr reduce)

(define S '(λ λ λ . ((2 . 0) . (1 . 0))))
(define K '(λ λ . 1))
(define I '(λ . 0))

;(app S K) ;; '((λ λ λ (2 . 0) 1 . 0) λ λ . 1)
;(r (app S K)) ;; '(λ λ ((λ λ . 1) . 0) 1 . 0)
;(r (r (app S K))) ;; '(λ λ (λ . 1) 1 . 0)
;(r (r (r (app S K)))) ;; '(λ λ . 0)
;(rr (app (app S K) K)) ;; '(λ . 0)
;(rr `((,S . (,K . (,S . ,I))) . (,S . (,K . ,K) . ,I))) ;; '(λ λ 0 . 1)

;;
;; Denotation
;;

(define (denote s)
  (cond
    [(not (pair? s))
     (cond
       [(eq? s 'S)
        S]
       [(eq? s 'K)
        K]
       [(eq? s 'I)
        I])]
    [(pair? s)
     (cons (denote (car s))
           (denote (cdr s)))]))

;;
;; Bracket Abstraction
;;

(define (rule1? s)
  (equal? s '(λ . 0)))

(define (free? s d)
  (cond
    [(term? s)
     (not (= s d))]
    [(app? s)
     (and (free? (car s) d)
          (free? (cdr s) d))]
    [(λ? s)
     (free? (cdr s) (+ d 1))]))

(define (rule2? s)
  (and (λ? s)
       (free? (cdr s) 0)))

(define (rule3? s)
  (and (λ? s)
       (app? (cdr s))))

(define (extract-M s)
  (car (cdr s)))

(define (extract-N s)
  (cdr (cdr s)))

(define flip '(λ λ 0 . 1))

(define (bracket s)
  (cond
    [(rule1? s)
     'I]
    [(rule2? s)
     (cons 'K (bracket (cdr s)))]
    [(rule3? s)
     (cons (cons 'S (bracket (abs (extract-M s))))
           (bracket (abs (extract-N s))))]
    [(and
      (λ? s)
      (λ? (cdr s)))
     (bracket (abs (unlift (bracket (cdr s)))))]
    [#t
     s]))

;(bracket (abs (app 0 0)))
;(bracket flip) ;; '((S K S . I) (S K . K) . I)
;(bracket '(λ 0 . 1)) ;; ((S I) K 1)

;(equal? (rr (denote (bracket flip))) flip)

;;
;; Even better bracket
;;

(define (comb? s)
  (match s
    ['S #t]
    ['K #t]
    ['I #t]
    [(cons A B) (and (comb? A)
                     (comb? B))]
    [_ #f]))

(define (better-bracket s)                    ; doesn't work yet
  (match s
    ;; λx(SKM)=SK (forall M)
    [(cons 'λ (cons 'S (cons 'K M)))
     (displayln "b1")
     (cons 'S 'K)]
    ;; λx.M = KM (x not in M)
    [(cons 'λ M)
     #:when (free? M 0)
     (displayln "b2")
     (cons 'K (better-bracket M))]
    ;; λx.x = I
    [(cons 'λ 0)
     'I]
    ;; λx(Mx)=M    (x not in M)
    [(cons 'λ (cons 0 (cons M 0)))
     #:when (free? M 0)
     (displayln "b4")
     (better-bracket M)]
    ;; λx(xMx) = λx.(SSKxM)
    [(cons 'λ (cons 0 (cons M 0)))
     (displayln "b5")
     (better-bracket `(((λ (S . S) . K) . 0) . ,(better-bracket M)))]
    ;; λx(M(NL)) = λx. S (λx.M) N L (M,N comb)
    [(cons 'λ (cons M (cons N L)))
     #:when (and (comb? M) (comb? N))
     (displayln "b6")
     (better-bracket `(λ ((S . ,(better-bracket (abs M))) . ,(better-bracket N)) . ,(better-bracket L)))]
    ;; λx(MN)L = λx.(S M (λx.L) N) (M,L comb)
    [(cons 'λ (cons (cons M N) L))
     #:when (and (comb? M) (comb? L))
     (displayln "b7")
     (better-bracket `(λ ((S . ,(better-bracket M)) . ,(better-bracket (abs L))) . ,(better-bracket N)))]
    ;; λx(ML)(NL) = λx.SMNL (MN comb)
    [(cons 'λ (cons (cons M L) (cons N L)))
     #:when (and (comb? M) (comb? N))
     (displayln "b8")
     (better-bracket `(λ ((,S . ,(better-bracket M)) . ,(better-bracket N)) . ,(better-bracket L)))]
    ;; λx M N = S (λx M) (λx N)
    [(cons 'λ (cons M N))
     (displayln "b9")
     `((S . ,(better-bracket (abs M))) . ,(better-bracket (abs N)))]
    ;;
    [`(λ λ . ,M)
     (displayln "b10")
     (better-bracket (cons 'λ (unlift (better-bracket (cons 'λ M)))))]
    [_  (displayln s) s]))

;(better-bracket I)
;(bracket flip)
;(better-bracket flip)
;;(unlift (better-bracket '(λ 0 . 1))) ;; should be '((S . I) K . 0)
;(unlift (bracket '(λ 0 . 1)))
;(bracket (abs '((S . I) K . 0))) ;; '((S K S . I) (S K . K) . I)
;(better-bracket flip)
;(better-bracket '(λ ((S . I) K . 0)))

;;
;; Simplification of SKI
;;

(define (interp s)
  (reduce (denote s)))

(define (simplify s)
  (bracket (interp s)))

;(simplify '(S . K)) ;; SK=KI
;(simplify '((S . K) . K)) ;; I
;(simplify '((S . K) . (I . S))) ;;I
;(simplify '(K K K K K K K K . K))
;(simplify '(S S S S S S S . S))

;;
;; Pretty print
;;

(define (pprint-paren s)
  (display "(")
  (pprint s)
  (display ")"))

(define (pprint-ppair s)
  (if (pair? s)
      (pprint-paren s)
      (pprint s)))

(define (pprint s)
  (cond
    [(pair? s)
     (pprint-ppair (car s))
     (pprint-ppair (cdr s))]
    [#t
     (display (cond
                [(eq? s 'S)
                 "S"]
                [(eq? s 'K)
                 "K"]
                [(eq? s 'I)
                 "I"]))]))

;(pprint (simplify '(S S . S)))
;(interp '(S S . S))
;(interp (simplify '(S S . S)))

;;
;; Evil things
;;
(define Y (abs (app (abs (app 0 0))
                    (abs (app 1 (app 0 0))))))
; (reduce (app Y I)) ;; Happy looping!

(simplify '((S . (K . (S . I))) . ((S . (K . K)) . I)))
(denote )