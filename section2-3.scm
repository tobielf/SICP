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

