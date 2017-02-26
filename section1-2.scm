(load "basic")
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


;;; Example fib.
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


;;; Example count-change
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

;;; Exercise 1.12
(define (pascal-triangle r c)
  (cond ((> c r) 0)
        ((= c 1) 1)
        ((= r c) 1)
        (else (+ (pascal-triangle (- r 1) (- c 1))
                 (pascal-triangle (- r 1) c)))))

;;; Exercise 1.14
;;; See Exercise-1-14.png
;;; O(2^n),exponential

;;; Exercise 1.15
(define (cube x)
  (* x x x))
(define (p x)
  (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))
;;; a. 5 times
;;; b. Theta(n)


;;; Example Exponentiation
;;; Recursive version( O(n) time, O(n) space)
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt-i b n)
  (expt-iter b n 1))

;;; Iterative version( O(n) time, O(1) space)
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b (- counter 1) (* b product))))

;;; Divide and conquer version( O(logN) time, O(logN) space)
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

;;; Exercise 1.16
(define (fast-expt-i b n)
  (fast-expt-iter b 1 n))

(define (fast-expt-iter b a n)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) a (/ n 2)))
        (else (fast-expt-iter b (* a b) (- n 1)))))

;;; Exercise 1.17
(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (fast-mul (double a) (halve b)))
        (else (+ a (fast-mul a (- b 1))))))

;;; Exercise 1.18
(define (fast-mul-i a b)
  (fast-mul-iter a b 0))

(define (fast-mul-iter a b x)
  (cond ((= b 0) x)
        ((even? b) (fast-mul-iter (double a) (halve b) x))
        (else (fast-mul-iter a (- b 1) (+ x a)))))

;;; Exercise 1.19
(define (fast-fib n)
  (fast-fib-iter 1 0 0 1 n))

(define (fast-fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count) (fast-fib-iter a 
                                      b 
                                      (+ (square p) (square q)) ; compute p'
                                      (+ (* 2 p q) (square q))  ; compute q'
                                      (/ count 2)))
        (else (fast-fib-iter (+ (* b q) (* a q) (* a p))
                             (+ (* b p) (* a q))
                             p
                             q
                             (- count 1)))))

;;; Example Greatest Common Divisors
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;; Exercise 1.20
;;; 0. gcd 206 40
;;; 1. gcd 40 (r 206 40)
;;; 2. gcd (r 206 40) (r 40 (r 206 40))
;;; 3. gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))
;;; 4. gcd (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
;;; 1-4, if (= b 0) called remainder 14 times, and step 4, called remainder 4 times.
;;; called remainder 18 times in normal-order evaluation

;;; 0. gcd 206 40
;;; 1. gcd 40 6 (r 206 40)
;;; 2. gcd 6 4 (r 40 6)
;;; 3. gcd 4 2 (r 6 4)
;;; 4. gcd 2 0 (r 4 2)
;;; and 4 times in applicative-order evaluation


