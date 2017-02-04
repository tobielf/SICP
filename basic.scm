(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;;; Exercise 1.3
(define (sum-of-larger-two-squares x y z)
  (cond ((and (> x z) (> y z)) (sum-of-squares x y))
        ((and (> y x) (> z x)) (sum-of-squares y z))
        (else (sum-of-squares x z))
  ))

;;; Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;;; Exercise 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

#|
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                      x)))
|#
;;; Else is a special symbol that can be used in the place of the <p>(predicate)
;;; in the final clause of a cond. This cause the cond to return as its value 
;;; the value of the corresponding <e> WHENEVER all previous clauses have been
;;; bypassed. In fact, any expression that ALWAYS evaluates to a true value
;;; could be used as the <p> here.