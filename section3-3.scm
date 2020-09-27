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

;;; Exercise 3.18
(define (cycle? l)
  (let ((counted '()))
    (define (traverse x)
      (cond ((null? x) #f)
            ((memq (car x) counted) #t)
            (else (begin 
                    (set! counted (cons (car x) counted))
                    (traverse (cdr x))))))
  (traverse l)))

;;; Exercise 3.19
(define (cycle? l)
  (define (cddr? l)
    (if (and (not (null? l))
             (not (null? (cdr l)))
             (not (null? (cddr l))))
      (cddr l)
      #f))
  (define (iter l ll)
    (cond ((eq? l ll) #t)
          ((not (cddr? ll)) #f)
          (else (iter (cdr l) (cddr ll)))))
  (if (cddr? l)
      (iter l (cddr l))
      #f))

;;; Exercise 3.20
(define (cons: x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)
(define (car: z) (z 'car))
(define (cdr: z) (z 'cdr))
(define (set-car!: z new-value)
  ((z 'set-car!) new-value)
  z)
(define (set-cdr!: z new-value)
  ((z 'set-cdr!) new-value)
  z)

(define x (cons: 1 2))
(define z (cons: x x))
(set-car!: (cdr: z) 17)
(car: x)

;         ____________________________________
;        | cons: car: cdr:                    |
; env -> | set-car!: set-cdr!:                |
;        | z------------------------|         |
;        | x-----|                  |         |
;        |_______|__________________|_________|
;                |      ^           |     ^
;               _v______|_        __v_____|____ 
;     E1(x) -> |   x = 1  | E2(z)|  x = E1(x)  |
;              |   y = 2  |      |  y = E1(x)  |
;              |  set-x!  |      |   set-x!    |
;              |  set-y!  |      |   set-y!    |
;              | dispatch |      |  dispatch   |
;              |__________|      |_____________|

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
      (cond ((empty-queue? queue)
             (set-front-ptr! queue new-pair)
             (set-rear-ptr! queue new-pair)
             queue)
            (else (set-cdr! (rear-ptr queue) new-pair)
                  (set-rear-ptr! queue new-pair)
                  queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue) 
         (error "DELETE! called with an empty queue" queue))
        (else 
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

;;; Exercise 3.21
(define (print-queue queue)
  (define (print-list list)
    (if (null? list)
        (display ")")
        (begin (display (car list))
               (display " ")
               (print-list (cdr list)))))
  (display "(")
  (print-list (front-ptr queue)))

;;; Exercise 3.22
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
      (define (empty-queue)
        (null? front-ptr))
      (define (front-queue)
        (if (empty-queue)
            front-ptr
            (car front-ptr)))
      (define (insert-queue item)
        (let ((new-pair (cons item '())))
            (cond ((empty-queue)
                   (set! front-ptr new-pair)
                   (set! rear-ptr new-pair))
                  (else (set-cdr! rear-ptr new-pair)
                        (set! rear-ptr new-pair)))
            front-ptr))
      (define (delete-queue)
        (cond ((empty-queue)
               front-ptr)
              (else 
                (set! front-ptr (cdr front-ptr))
                front-ptr)))
      (define (dispatch m)
        (cond ((eq? m 'insert-queue) insert-queue)
              ((eq? m 'delete-queue) delete-queue)
              ((eq? m 'empty-queue) empty-queue)
              ((eq? m 'front-queue) front-queue)
              (else (error "Undefined operation -- dispatch" m))))
  dispatch))

(define (empty-queue? queue)
  ((queue 'empty-queue)))
(define (front-queue queue)
  ((queue 'front-queue)))
(define (delete-queue! queue)
  ((queue 'delete-queue)))
(define (insert-queue! queue item)
  ((queue 'insert-queue) item))

;;; Exercise 3.23
(define (make-deque) (cons '() '()))
(define (empty-deque? deque) (or (null? (front-ptr deque))
                                 (null? (rear-ptr deque))))
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (car (front-ptr deque)))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (car (car (rear-ptr deque)))))
(define (front-insert-deque! deque item)
  (let ((new-pair (cons (cons item '()) '())))
      (cond ((empty-deque? deque)
             (set-front-ptr! deque new-pair)
             (set-rear-ptr! deque new-pair))
            (else (set-cdr! (car new-pair) (front-ptr deque))
                  (set-cdr! (front-ptr deque) new-pair)
                  (set-front-ptr! deque new-pair)))))
(define (rear-insert-deque! deque item)
  (let ((new-pair (cons (cons item '()) '())))
      (cond ((empty-deque? deque)
             (set-front-ptr! deque new-pair)
             (set-rear-ptr! deque new-pair))
            (else (set-cdr! (car (rear-ptr deque)) new-pair)
                  (set-cdr! new-pair (rear-ptr deque))
                  (set-rear-ptr! deque new-pair)))))
(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE FRONT! called with an empty deque" deque))
        (else (set-front-ptr! deque (cdr (car (front-ptr deque))))
              (or (empty-deque? deque) (set-cdr! (front-ptr deque) '())))))
(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE REAR! called with an empty deque" deque))
        (else (set-rear-ptr! deque (cdr (rear-ptr deque)))
              (or (empty-deque? deque) (set-cdr! (car (rear-ptr deque)) '())))))

;;; Defined as procedure
(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))
      (define (empty-deque)
        (or (null? front-ptr) (null? rear-ptr)))
      (define (front-deque)
        (if (empty-deque)
            '()
            (car (car front-ptr))))
      (define (rear-deque)
        (if (empty-deque)
            '()
            (car (car rear-ptr))))
      (define (front-insert-deque item)
        (let ((new-pair (cons (cons item '()) '())))
            (cond ((empty-deque)
                   (set! front-ptr new-pair)
                   (set! rear-ptr new-pair))
                  (else (set-cdr! (car new-pair) front-ptr)
                        (set-cdr! front-ptr new-pair)
                        (set! front-ptr new-pair)))))
      (define (rear-insert-deque item)
        (let ((new-pair (cons (cons item '()) '())))
            (cond ((empty-deque) 
                   (set! front-ptr new-pair)
                   (set! rear-ptr new-pair))
                  (else (set-cdr! (car rear-ptr) new-pair)
                        (set-cdr! new-pair rear-ptr)
                        (set! rear-ptr new-pair)))))
      (define (front-delete-deque)
        (cond ((empty-deque) '())
              (else (set! front-ptr (cdr (car front-ptr)))
                    (or (empty-deque) (set-cdr! front-ptr '())))))
      (define (rear-delete-deque)
        (cond ((empty-deque) '())
              (else (set! rear-ptr (cdr rear-ptr))
                    (or (empty-deque) (set-cdr! (car rear-ptr) '())))))
      (define (dispatch m)
        (cond ((eq? m 'empty-deque) empty-deque)
              ((eq? m 'front-deque) front-deque)
              ((eq? m 'rear-deque) rear-deque)
              ((eq? m 'front-insert-deque) front-insert-deque)
              ((eq? m 'rear-insert-deque) rear-insert-deque)
              ((eq? m 'front-delete-deque) front-delete-deque)
              ((eq? m 'rear-delete-deque) rear-delete-deque)
              (else (error "Undefined operation -- dispatch" m))))
  dispatch))

(define (empty-deque? deque) ((deque 'empty-deque)))
(define (front-deque deque) ((deque 'front-deque)))
(define (rear-deque deque) ((deque 'rear-deque)))
(define (front-insert-deque! deque item) ((deque 'front-insert-deque) item))
(define (rear-insert-deque! deque item) ((deque 'rear-insert-deque) item))
(define (front-delete-deque! deque) ((deque 'front-delete-deque)))
(define (rear-delete-deque! deque) ((deque 'rear-delete-deque)))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
      (if record
          (cdr record)
          #f)))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
      (if record
          (set-cdr! record value)
          (set-cdr! table
                    (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))
