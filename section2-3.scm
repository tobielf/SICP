(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;;; Exercise 2.53
(list 'a 'b 'c)
;Value: (a b c)

(list (list 'george))
;Value: ((george))

(cdr `((x1 x2) (y1 y2)))
;Value: ((y1 y2))

(cadr `((x1 x2) (y1 y2)))
;Value: (y1 y2)

(pair? (car `(a short list)))
;Value: #f

(memq `red `((red shoes) (blue socks)))
;Value: #f

(memq `red `(red shoes blue socks))
;Value: (red shoes blue socks)

;;; Exercise 2.54
(define (equal?* l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((or (null? l1) (null? l2)) #f)
        ((not (eq? (car l1) (car l2))) #f)
        (else (equal?* (cdr l1) (cdr l2)))))

(equal?* `(this) `(this is))
(equal?* `(this is a list) `(this is a list))
(equal?* `(this is a list) `(this (is a) list))

;;; Exercise 2.55
(car ''abracadabra)
; The second quote has been treated as the object after the first quote.

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) 
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum 
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(deriv '(+ x 3) 'x)
;Value: (+ 1 0)

(deriv '(* x y) 'x)
;Value: (+ (* x 0) (* 1 y))

(deriv '(* (* x y) (+ x 3)) 'x)
;Value: (+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;;; Exercise 2.56
(define (make-exponentiation b exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) b)
        (else (list '** b exp))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) 
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum 
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product (make-exponentiation (base exp) (- (exponent exp) 1))
                                     (deriv (base exp) var))))
        (else (error "unknown expression type -- DERIV" exp))))

;;; Exercise 2.57
(define (make-arbitrary-sum a1 . rest)
  (if (> (length rest) 1)
      (cons '+ (cons a1 rest))
      (let ((a2 (car rest)))
      (make-sum a1 a2))))

(define (addend s) (cadr s))

(define (augend s)
  (if (= (length (cddr s)) 1)
    (caddr s)
    (cons (car s) (cddr s))))

(define (make-arbitrary-product m1 . rest)
  (if (> (length rest) 1)
      (cons '* (cons m1 rest))
      (let ((m2 (car rest)))
      (make-product m1 m2))))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (if (= (length (cddr p)) 1)
    (caddr p)
    (cons (car p) (cddr p))))


(deriv '(* x y (+ x 3)) 'x)
(deriv (make-arbitrary-product 'x 'y (make-arbitrary-sum 'x 3)) 'x)

;;; Exercise 2.58
;a
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

;b is harder, so I decided to do later.

;;; Representing sets as unordered lists.
; O(n) complexity.
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; O(n) complexity.
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

; O(n^2) complexity.
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) `())
        ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;;; Exercise 2.59
; O(n^2) complexity.
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))
