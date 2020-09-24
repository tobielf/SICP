(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

;(define (last-pair x)
;  (if (null? (cdr x))
;      x
;      (last-pair (cdr x))))

;;; Exercise 3.12

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

;> z
;(a b c d)
;> (cdr x)
;(b)

(define w (append! x y))

;> w
;(a b c d)
;> (cdr x)
;(b c d)

;;; Exercise 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))
; z -> ['a][*] -> ['b][*] -> ['c][*]
;       ^                         |
;       |_________________________|

;(last-pair z)
; This will fall into endless recursion. Since the pointer pointed back.
; Got "Exception in last-pair: (a b c a b c ...) is circular" under Chez Scheme.
