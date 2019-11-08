#lang racket

(let ((x 36875131794129999827197811565225474825492979968971970996283137471637224634055579)
      (y 154476802108746166441951315019919837485664325669565431700026634898253202035277999)
      (z 4373612677928697257861252602371390152816537558161613618621437993378423467772036))
  (+ (/ x (+ y z))
     (/ y (+ x z))
     (/ z (+ x y))))

(define (continued-fraction r)
  (let* ((i (floor r))
         (f (- r i)))
    (displayln i)
    (unless (= f 0)
      (continued-fraction (/ 1 f)))))

(define (convergent a l)
  (if (null? l)
      a
      (+ a (/ 1 (convergent (car l) (cdr l))))))

(define (make-ec p q) (cons p q)) ;; y^2 = x^3 + px + q
(define (ec-p ec) (car ec))
(define (ec-q ec) (cdr ec))
(define (make-pt x y) (cons x y)) ;; point (x, y)
(define (pt-x p) (car p))
(define (pt-y p) (cdr p))
(define ec-O '())

(define (ec-neg P) (make-pt (pt-x P) (- 0 (pt-y P))))
(define (ec-add ec P Q)
  (if (= (pt-x P) (pt-x Q))
      ;; xP = xQ
      (if (not (= (pt-y P) (pt-y Q)))
          ec-O
          ;; P = Q
          (let* ((xP (pt-x P))
                 (yP (pt-y P))
                 (s (/ (+ (* 3 xP xP) (ec-p ec)) (* 2 yP)))
                 (xR (- (* s s) (* 2 xP)))
                 (yR (- (* s (- xP xR)) yP)))
            (make-pt xR yR)))
      ;; xP /= xQ
      (let* ((xP (pt-x P))
             (yP (pt-y P))
             (xQ (pt-x Q))
             (yQ (pt-y Q))
             (s (/ (- yP yQ) (- xP xQ)))
             (xR (- (* s s) xP xQ))
             (yR (- (* s (- xP xR)) yP)))
        (make-pt xR yR))))

(define (ec-mul ec p n)
  (if (= n 1)
      p
      (ec-add ec p (ec-mul ec p (- n 1)))))

(define EC (make-ec -11209/48 1185157/864))
(define P (make-pt -191/12 65/2))
(ec-mul EC P 9)



(define (gcd a b)
  (if (= a 0)
      b
      (gcd (remainder b a) a)))

(define (lcm a b)
  (/ (* a b) (gcd a b)))


(define (to-original P)
  (let* ((x (pt-x P))
         (y (pt-y P))
         (X (+ (* 1/182 x) (* -1/91 y) -277/2184))
         (Y (+ (* 1/182 x) (* 1/91 y) -277/2184))
         (Z (+ (* 6/91 x) -95/182))
         (k (lcm (lcm (denominator X) (denominator Y)) (denominator Z))))
    (list (* -1 k X) (* -1 k Y) (* -1 k Z))))
(to-original (ec-mul EC P 9))



;;;; Generalized Continued Fraction -- Convergents
(define (sqrt-next Ci-2 Ci-1 i n a)
  (if (= i 0)
      Ci-1
      (sqrt-next Ci-1
                 (+ (* 2 a Ci-1) (* (- n (* a a)) Ci-2))
                 (- i 1) n a)))
(define (A i n a) (sqrt-next 1 a i n a))
(define (B i n a) (sqrt-next 0 1 i n a))
(define (exact-conv i n a) (/ (A i n a) (B i n a)))
(define (conv i n a) (exact->inexact (exact-conv i n a)))

(define (pell-fundamental n a i)
  (let* ((c (exact-conv i n a))
         (h (numerator c))
         (k (denominator c)))
    (if (= 1 (- (* h h) (* n k k)))
        (cons h k)
        (pell-fundamental n a (+ i 1)))))

(define (pell n)
  (pell-fundamental n (exact-floor (sqrt n)) 0))

(define (pell-special a)
  (pell-fundamental (+ 1 (* a a)) a 0))

(define (check r)
  (let* ((i (exact-floor (sqrt r)))
         (k (- r (* i i)))
         (k2 (* k k)))
    (cons k2 (- r (* i i)))))

;;; Nope, This DOESN"T WORK!
;;; The continuants of GCF are usually not the same as those of CF
;;; And unfortunately the answer sometimes don't show up in the continuants... (or very large so that they are not found in reasonable time.)
