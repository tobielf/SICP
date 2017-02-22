;;; Exercise 1.9
;;; recursive
;;;(define (+ a b)
;;;  (if (= a 0)
;;;    b
;;;    (inc (+ (dec a) b))))

;;; iterative
;;;(define (+ a b)
;;;  (if (= a 0)
;;;    b
;;;    (+ (dec a) (inc b))))

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

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) 
                 (fib (- n 2))))))

(define (fib-i n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;;; Exercise 1.11
(define (f_ex11 n)
  (cond ((< n 3) n)
        ((>= n 3) (+ (f_ex11 (- n 1)) 
                     (* 2 (f_ex11 (- n 2)))
                     (* 3 (f_ex11 (- n 3)))))))

(define (f-ex11 n)
  (cond ((< n 3) n)
        (else (f-ex11-iter 2 1 0 n))))

(define (f-ex11-iter a b c count)
  (if (< count 3)
      a
      (f-ex11-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
