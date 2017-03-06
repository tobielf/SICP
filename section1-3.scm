(load "basic")

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


