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

;;; Exercise 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
            (set-cdr! x y)
            (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
; v -> ['a][*] -> ['b][*] -> ['c][*] -> ['d][*] -> nil

(define w (mystery v))
; loop(x '())
; v/x -> ['a][*]-> ['b][*] -> ['c][*] -> ['d][*] -> nil
; y -> nil
; (let ((temp (cdr x)))
; temp -> ['b][*] -> ['c][*] -> ['d][*] -> nil  (make a new copy after (cdr x))

;(set-cdr! x y)
; v/x -> ['a][*]-> nil ['b][*] -> ['c][*] -> ['d][*] -> nil  (this one will be gc)


; loop(temp x)
; x -> ['b][*] -> ['c][*] -> ['d][*] -> nil (temp from last iteration)
; y -> ['a][*]-> nil (x from last iteration)
; (let ((temp (cdr x)))
; temp -> ['c][*] -> ['d][*] -> nil  (make a new copy after (cdr x))

;(set-cdr! x y)
; x -> ['b][*] -> ['a][*]-> nil 

; repeat until x is '() and the y is:
; y -> ['d][*] -> ['c][*] -> ['b][*] -> ['a][*]-> nil 
; However, v only point to one element after calling mystery.

;> w
;(d c b a)
;> v
;(a)

; Sharing and identity.
(define x (list 'a 'b))
(define z1 (cons x x))

(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

;;; Exercise 3.15
;> z1
;((a b) a b)
;> (set-to-wow! z1)
;((wow b) wow b)

; z1 ->[*][*]
;       |  |
;       v  v
; x  ->[*][*] -> [*][/]
;       |         |
;       v         v
;     [wow]      [b]

;> z2
;((a b) a b)
;> (set-to-wow! z2)
;((wow b) a b)

; z2 ->[*][*] -> [*][*] -> [*][/]
;       |         |         |
;       |         v         v
;       |        [a]       [b]
;       |                   ^
;       |                   |
;       -------> [*][*] -> [*][/]
;                 |
;                 v
;               [wow]

;;; Exercise 3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))
(define x '(a))
(define y (cons x x))
; y -> [x][*] -> [x][*]
;       |____________|
;       v
;      [a][*] -> [*][/]
(count-pairs (list y))
; p -> [y][*] -> [*][/]
;       |
;       v
;      [x][*] -> [x][*]
;       |____________|
;       v
;      [a][*] -> [*][/]
(count-pairs (cons y y))
; p -> [y][*] -> [y][*]
;       |____________| 
;       v
;      [x][*] -> [x][*]
;       |____________|
;       v
;      [a][*] -> [*][/]
(count-pairs (set-cdr! (list y) x))
(define z (make-cycle (list 'a 'b 'c)))

;;; Exercise 3.17
(define (count-pairs x)
  (let ((counted '()))
    (define (count x)
      (if (or (not (pair? x)) (memq x counted))
        0
        (begin
          (set! counted (cons x counted))
          (+ (count (car x))
             (count (cdr x))
             1))))
    (count x)))
