(load "section1-2")

;;; Example: Arithmetic Operations for Rational Numbers
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define x (cons 1 2))

(define (make-rat n d)
  (cons n d))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)
(define one-third (make-rat 1 3))
(print-rat one-third)
(print-rat (add-rat one-half one-third))
(print-rat (sub-rat one-half one-third))
(print-rat (add-rat one-third one-third))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(print-rat (add-rat one-third one-third))

;;; Exercise 2.1:
(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (if (or (and (< n 0) (< d 0)) (and (> n 0) (> d 0)))
        (cons (/ (abs n) g) (/ (abs d) g))
        (cons (/ (- (abs n)) g) (/ (abs d) g)))
    ))

(define (make-rat n d)
  (cons n d))

(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))

(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))

;;; Constraining the dependence on the representation to a few interface 
;;; procedures helps us design programs as well as modify them.

;;; Exercise 2.2:
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (make-point (/ (+ (x-point end) (x-point start)) 2) 
                (/ (+ (y-point end) (y-point start)) 2)
    )
  )
)

(define (segment-length segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (sqrt (+ (square (- (x-point start) (x-point end)))
             (square (- (y-point start) (y-point end)))
          )
    )
  )
)

;;; Exercise 2.3:
;;; Rectangles (Width, Length)
(define (make-rect width length)
  (cons width length))

(define (get-width rect)
  (car rect))

(define (get-length rect)
  (cdr rect))

(define (rectangle-perimeter rect)
  (let ((width (get-width rect))
        (length (get-length rect)))
    (* (+ (segment-length width) (segment-length length)) 2)
  )
)

(define (rectangle-area rect)
  (let ((width (get-width rect))
        (length (get-length rect)))
    (* (segment-length width) (segment-length length))
  )
)

;;; Data as defined by some collection of selectors and constructors, 
;;; together with specified conditions that these procedures must
;;; fulfill in order to be a valid representation.

;;; Powerful Pair Glue Everything together
;;;

;(define (cons x y)
;  (define (dispatch m)
;    (cond ((= m 0) x)
;          ((= m 1) y)
;          (else (error "Argument not 0 or 1 -- CONS" m))))
;  dispatch
;)

;(define (car z)
;  (z 0)
;)

;(define (cdr z)
;  (z 1)
;)

;;; The procedural representation, although obscure, is a perfectly adequate way
;;; to represent pairs, since it fulfills the only conditions that pairs,
;;; since it fulfills the only conditions that pairs need to fulfill.
;;; The above example also demonstrates that the ability to manipulate procedures
;;; as objects automatically provides the ability to represent compound data.
;;; This style of programming is often called message passing.

;;; Exercise 2.4:
;;;
(define (cons-ex x y)
  (lambda (m) (m x y)))
(define (car-ex z)
  (z (lambda (p q) p)))
(define (cdr-ex z)
  (z (lambda (p q) q)))

;;; Exercise 2.5
(define (cons-int a b)
  (* (expt 2 a)
     (expt 3 b)
  )
)

; helper function to extract the index of given base (d)
(define (n-divs n d)
  (define (iter x count)
    (if (= (remainder x d) 0)
        (iter (/ x d) (+ count 1))
        count))
  (iter n 0)
)

(define (car-int z)
  (n-divs z 2))

(define (cdr-int z)
  (n-divs z 3))



