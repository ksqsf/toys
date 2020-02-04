#lang racket

;;; Helper functions
(define fresh-name-counter -1)
(define (fresh-name)
  (set! fresh-name-counter (+ fresh-name-counter 1))
  (string->symbol
   (string-append "x" (number->string fresh-name-counter))))
(define (acons x y a-list)
  (cons (cons x y) a-list))
(define (all test? ls)
  (or (null? ls)
      (and (test? (car ls))
           (all test? (cdr ls)))))
(define list-difference
  (lambda (big small)
    (let loop ((ls big))
      (if (eq? ls small)
          '()
          (cons (car ls)
                (loop (cdr ls)))))))
(define (iota p)
  (let loop ((p p) (ls '()))
    (if (= p 0)
        ls
        (loop (- p 1)
              (cons (- p 1) ls)))))

;;; Emitter
(define (emit-power x n)
  (cond ((= n 0) 1)
        ((odd? n) (emit-* x (emit-power x (- n 1))))
        (else (emit-square (emit-power x (/ n 2))))))

(define (emit-square x)
  (emit-* x x))

(define (emit-* x y)
  `(* ,x ,y))

(define (emit-+ x y)
  `(+ ,x ,y))

;;; Symbolic Execution
(define (poly-value coeffs x)
  (foldl (lambda (value coeff)
           (+ (* value x) coeff))
         0
         coeffs))

;;; Common Subexpression Elimination
(define (with-cse-old receiver)
  (let ((bindings '()))

    (define (cseify emitter)
      (lambda operands
        (let ((exp (apply emitter operands)))
          (cond ((or (symbol? exp) (number? exp)) exp)
                ((assoc exp bindings) => cadr)
                (else (let ((name (fresh-name)))
                        (set! bindings (cons (list exp name) bindings))
                        name))))))

    (let ((exp (receiver cseify)))
      `(let* ,(reverse (map reverse bindings))
         ,exp))))

(define (emit-poly-value coeffs x)
  (with-cse-old
   (lambda (cseify)
     (let ((* (cseify emit-*))
           (+ (cseify emit-+)))
       (foldl (lambda (value coeff)
                (+ (* value x) coeff))
              0
              coeffs)))))

(emit-poly-value '(5 0 2) 2)


;;; From interpreter to compiler
(define *simplification-rules*
  '(((+ ?x ?x)        (* 2 ?x))
    ((* ?s ?n)        (* ?n ?s))
    ((* ?n (* ?m ?x)) (* (* ?n ?m) ?x))
    ((* ?x (* ?n ?y)) (* ?n (* ?x ?y)))
    ((* (* ?n ?x) ?y) (* ?n (* ?x ?y)))))

(define (simplify exp)
  (if (pair? exp)
      (simplify-exp (map simplify exp))
      exp))

(define (simplify-exp-old exp)
  (or (arithmetic-eval exp)
      (rule-based-translator exp *simplification-rules*
                             (lambda (env response)
                               (simplify (sublis env response))))
      exp))

(define (rule-based-translator input rules action)
  (let checking ((rules rules))
    (and (not (null? rules))
         (let ((pattern (rule-pattern (car rules)))
               (response (rule-response (car rules))))
           (or (pat-match pattern input '()
                          (lambda (env) (action env response)))
               (checking (cdr rules)))))))

(define rule-pattern car)
(define rule-response cadr)

(define (pat-match pattern input env succeed)
  (cond ((not env) #f)
        ((pair? pattern)
         (and (pair? input)
              (pat-match (cdr pattern) (cdr input)
                         (pat-match (car pattern) (car input) env
                                    succeed))))
        ((variable? pattern)
         (match-variable pattern input env succeed))
        (else
         (and (equal? pattern input)
              (succeed env)))))

(define (match-variable var input env succeed)
  (cond ((assq var env)
         => (lambda (pair)
              (and (equal? input (cdr pair))
                   (succeed env))))
        ((memq var '(?m ?n))
         (and (number? input)
              (succeed (acons var input env))))
        (else (succeed (acons var input env)))))

(define (variable? x)
  (and (symbol? x)
       (< 0 (string-length (symbol->string x)))
       (char=? #\? (string-ref (symbol->string x) 0))))

(define (sublis env exp)
  (cond ((null? exp) '())
        ((pair? exp)
         (cons (sublis env (car exp))
               (sublis env (cdr exp))))
        ((assq exp env) => cdr)
        (else exp)))

(define (arithmetic-eval exp)
  (and (all number? (exp-args exp))
       (or (memq (exp-op exp) '(+ - * /))
           (and (eq? (exp-op exp) '^)
                (integer? (cadr (exp-args exp)))))
       (apply (eval-primitive-name (exp-op exp))
              (exp-args exp))))

(define (eval-primitive-name op)
  (cadr (assq op op-meanings)))

(define op-meanings
  `((+ ,+) (- ,-) (* ,*) (/ ,/) (^ ,expt)))

(define exp-op car)
(define exp-args cdr)

;; Partial evaluate simplify + rules
(define dynamic-tag (list 'dynamic))
(define (emit code) (list dynamic-tag code))
(define (dynamic.code dynamic) (cadr dynamic))

(define (dynamic? obj) (and (pair? obj) (eq? dynamic-tag (car obj))))
(define (static? obj) (not (dynamic? obj)))

(define (as-code obj)
  (cond ((dynamic? obj)
         (dynamic.code obj))
        ((or (null? obj) (pair? obj) (symbol? obj))
         (list 'quote obj))
        (else obj)))

(define (make-emitter op op-name)
  (lambda operands
    (if (all static? operands)
        (apply op operands)
        (emit `(,op-name ,@(map as-code operands))))))

(define (with-cse receiver)
  (let ((bindings '()))

    (define (run emitter-thunk)
      (let* ((old-bindings bindings)
             (value (emitter-thunk))
             (result (wrap-let* (list-difference bindings old-bindings)
                                value)))
        (set! bindings old-bindings)
        result))

    (define (cseify emitter)
      (lambda operands
        (let ((value (apply emitter operands)))
          (if (static? value)
              value
              (let ((code (dynamic.code value)))
                (cond ((symbol? code) value)
                      ((assoc code bindings)
                       => (lambda (pair) (emit (cadr pair))))
                      (else (let ((name (fresh-name)))
                              (set! bindings (cons (list code name) bindings))
                              (emit name)))))))))

    (define (wrap-let* bindings exp)
      (emit `(let* ,(reverse (map reverse bindings))
               ,(as-code exp))))

    (let ((value (receiver cseify run)))
      (as-code (wrap-let* bindings value)))))

(define (compile-rules)
  (with-cse
   (lambda (cse run)
     (let ((cons    (cse (make-emitter cons 'cons)))
           (car     (cse (make-emitter car 'car)))
           (cdr     (cse (make-emitter cdr 'cdr)))
           (pair?   (cse (make-emitter pair? 'pair?)))
           (number? (cse (make-emitter number? 'number?)))
           (equal?  (cse (make-emitter equal? 'equal?)))
           (%and    (lambda (test then-proc)
                      (emit-and test (run then-proc))))
           (%or     (lambda (test else-proc)
                      (emit-or test (run else-proc)))))

       (define (rule-based-translator input rules action)
         (let checking ((rules rules))
           (and (not (null? rules))
                (let ((pattern (rule-pattern (car rules)))
                      (response (rule-response (car rules))))
                  (%or (pat-match pattern input '()
                                  (lambda (env) (action env response)))
                       (lambda ()
                         (checking (cdr rules))))))))

       (define (pat-match pattern input env succeed)
         (cond ((pair? pattern)
                (%and (pair? input)
                      (lambda ()
                        (pat-match (car pattern) (car input) env
                                   (lambda (env)
                                     (pat-match (cdr pattern) (cdr input) env
                                                succeed))))))
               ((variable? pattern)
                (match-variable pattern input env succeed))
               (else
                (%and (equal? pattern input)
                      (lambda () (succeed env))))))

       (define (match-variable var input env succeed)
         (cond ((assq var env)
                => (lambda (pair)
                     (%and (equal? (cdr pair) input)
                           (lambda () (succeed env)))))
               ((memq var '(?m ?n))
                (%and (number? input)
                      (lambda () (succeed (acons var input env)))))
               (else (succeed (acons var input env)))))

       (define (sublis a-list exp)
         (cond ((null? exp) '())
               ((pair? exp)
                (cons (sublis a-list (car exp))
                      (sublis a-list (cdr exp))))
               ((assq exp a-list) => cdr)
               (else exp)))

       (rule-based-translator (emit 'subject)
                              *simplification-rules*
                              sublis)))))

(define (emit-and . operands)
  (emit `(and ,@(map as-code operands))))

(define (emit-or . operands)
  (emit `(or ,@(map as-code operands))))

(define (simplify-exp exp)
  (cond ((arithmetic-eval exp))
        ((apply-rules exp) => simplify)
        (else exp)))

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))
(define apply-rules
  (eval `(lambda (subject) ,(compile-rules)) ns))


;;; Discrete Fourier Transform
;;; w/ Danielson-Lanczos lemma

(define (dft inputs)
  (let ((n (length inputs)))
    (map (lambda (i) (poly inputs i n))
         (iota n))))

(define (poly coeffs p n)
  (if (null? (cdr coeffs))
      (car coeffs)
      (alternate coeffs
                 (lambda (evens odds)
                   (let ((p2 (modulo (* 2 p) n)))
                     (complex+ (poly evens p2 n)
                               (complex* (root-of-unity p n)
                                         (poly odds p2 n))))))))

(define (alternate ls receiver)
  (do ((ls     ls   (cddr ls))
       (evens  '()  (cons (car ls) evens))
       (odds   '()  (cons (cadr ls) odds)))
      ((null? ls)
       (receiver (reverse evens) (reverse odds)))))

(define (make-complex re im) (list re im))
(define re car)
(define im cadr)

(define (complex+ a b)
  (make-complex (+ (re a) (re b))
                (+ (im a) (im b))))
(define (complex* a x)
  (make-complex (- (* (re a) (re x)) (* (im a) (im x)))
                (+ (* (im a) (re x)) (* (re a) (im x)))))
(define (root-of-unity i n)
  (let ((theta (* (* 2 pi) (/ i n))))
    (make-complex (cos theta) (sin theta))))

