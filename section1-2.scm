;;; Exercise 1.9
;;; recursive
(define (+ a b)
  (if (= a 0)
    b
    (inc (+ (dec a) b))))

;;; iterative
(define (+ a b)
  (if (= a 0)
    b
    (+ (dec a) (inc b))))

;;; Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
;;; (A 0 5)=10 (A 0 N) = 2 * N
;;; (A 1 10)=1024 (A 1 N) = 2 ^ N
;;; (A 2 4)=65536 (A 2 N) = 2 ^ (A 2 N-1), (A 2 1) = 2

