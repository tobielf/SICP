(load "section2-1")

(define one-through-four (list 1 2 3 4))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0)
)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;;; Exercise 2.17
(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))

;;; Exercise 2.18
(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))

;;; Exercise 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (no-more? coin-values)
  (null? coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount 
                   (first-denomination coin-values)) 
                coin-values)))))

;;; Does the order of the list coin-values affect the answer produced by cc?
;;; No, it doesn't affect. Because the way we are searching will cover all combinations.

;;; Exercise 2.20
(define (same-parity target . rest)
  (define (same? item)
    (if (= (remainder target 2) (remainder item 2))
        (list item)
        (list)))
  (define (same-parity-recurse items)
    (if (null? items)
        items
        (append (same? (car items)) (same-parity-recurse (cdr items))) ))
  (append (list target) (same-parity-recurse rest)))

(define (same-parity target . rest)
  (define (same? item ans)
    (if (= (remainder target 2) (remainder item 2))
        (append ans (list item))
        ans))
  (define (same-parity-iter items ans)
    (if (null? items)
        ans
        (same-parity-iter (cdr items) (same? (car items) ans))))
  (same-parity-iter rest (list target)))

(define (scale-list items factor)
  (if (null? items)
      items
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(define (map proc items)
  (if (null? items)
      items
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

;;; Exercise 2.21
(define (square-list items)
  (if (null? items)
      items
      (cons (* (car items) (car items))
            (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

(define (square-list items)
  (map square items))

;;; Exercise 2.22
;(define (square-list items)
;  (define (iter things answer)
;    (if (null? things)
;        answer
;        (iter (cdr things)
;              (cons (square (car things))
;                    answer))))
;  (iter items (list)))

;;; here cons append the previous answer after the new answer, so we will get
;;; a reversed order answer.

;(define (square-list items)
;  (define (iter things answer)
;    (if (null? things)
;        answer
;        (iter (cdr things)
;              (cons answer (square (car things))
;                    ))))
;  (iter items (list)))

;;; cons creates a closure (pairs) of two elements so the answer will be a list of lists
;;; (((((() . 1) . 4) . 9) . 16) . 25)
;;; We need to reverse the order and use append to glue two lists to one.

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer (list (square (car things)))
                    ))))
  (iter items (list)))

;;; Exercise 2.23
(define (for-each proc items)
  (cond ((null? items) #t)
        (else (proc (car items))
              (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x)) 
          (list 57 321 88))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;;; Exercise 2.24
(list 1 (list 2 (list 3 4)))
;Value: (1 (2 (3 4)))
; -->[ ][ ]-->[ ][ ]-->[ ][ ]-->[ ][/]
;     |        |        |        |
;     v        v        v        v
;    [1]      [2]      [3]      [4]


;;; Exercise 2.25
(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))
(car (car (list (list 7))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))
