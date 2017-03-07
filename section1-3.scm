(load "basic")
(load "section1-2")

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n)
  (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x)
  x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

;;; Exercise 1.29
(define (integral-simpson f a b n)
  (define (h-func k)
    (* (/ (- b a) n) k))
  (define (integral-term k)
    (cond ((or (= k 0) (= k n)) (f (+ a (h-func k))))
          ((even? k) (* (f (+ a (h-func k))) 2))
          (else (* (f (+ a (h-func k))) 4))))
  (* (h-func (/ 1 3)) (sum integral-term 1 inc n)))

;;; Exercise 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;;; Exercise 1.31
;;; a. recursive method
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) 
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(define (pi n)
  (define (term a)
    (if (= a 2)
        2
        (square a)))
  (define (inc-by-two a)
    (+ a 2))
  (exact->inexact (/ (* 4 (/ (product term 2 inc-by-two n)
       (product term 3 inc-by-two n))) n)))

;;; b. iterative method
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;;; Exercise 1.32
;;; a. recursive method
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;;; b. iterative method
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

;;; Exercise 1.33
(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (combiner (if (filter a)
                    (term a)
                    null-value) 
                (filtered-accumulate combiner null-value term (next a) next b filter))))

(define (sum-squares-prime a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (product-relatively-prime n)
  (define (relatively-prime a)
    (= (gcd a n) 1))
  (filtered-accumulate * 1 identity 1 inc n relatively-prime))

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (if (filter a)
            (term a)
            null-value)))))
  (iter a null-value))

;;; "lambda" expression and "let" syntax sugar
;;; previous example without helper function
(define (pi-sum a b)
  (sum (lambda (x) (+ x 4)) 
       a
       (lambda (x) (/ 1.0 (* x (+ x 2))))
       b))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

;;; with helper function.
(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

;;; with anonymous lambda expression
(define (f x y)
  ((lambda (a b) 
     (+ (* x (square a))
        (* y b)
        (* a b)))
  (+ 1 (* x y))
  (- 1 y)))

;;; using let syntax sugar
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;;; Exercise 1.34
(define (f g)
  (g 2))

;;; (f f) => (2 2), 2 is not a applicable object.
;;; It's a applicative-order evaluation, it will using second f,
;;; replace the g in first f, turns in to (f 2), which will finally
;;; turn into (2 2).

;;; Example: half-interval method
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value) (search f neg-point midpoint))
                ((negative? test-value) (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value)) 
            (search f a b))
          ((and (negative? b-value) (positive? a-value)) 
            (search f b a))
          (else
            (error "Values are not of opposite sign" a b)))))

;;; Example: finding fixed points of functions
;;; average damping
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; oscillating one.
;(define (sqrt x)
;  (fixed-point (lambda (y) (/ x y)) 1.0))


; notice that y = (1/2)(y + x/y) is a simple 
; transformation of the equation y = x/y.
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;;; Exercise 1.35
(define (golden-ratio)
  (fixed-point (lambda (x) (average x (+ 1 (/ 1 x)))) 1.0))

;;; Exercise 1.36
(define (fixed-point-e36 f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (ex1-36-without)
  (fixed-point-e36 (lambda (x) (/ (log 1000) (log x))) 2.0))

(define (ex1-36-with)
  (fixed-point-e36 (lambda (x) (average x (/ (log 1000) (log x)))) 2.0))
