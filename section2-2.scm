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

(define (map* proc items)
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


;;; Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
;Value: (1 2 3 4 5 6)
(cons x y)
;Value: ((1 2 3) 4 5 6)
(list x y)
;Value: ((1 2 3) (4 5 6))

;;; Exercise 2.27
(define x (list (list 1 2) (list 3 4)))
;Value: ((1 2) (3 4))

;(define (deep-reverse items)
;  (cond ((null? items) items)
;        ((not (pair? items)) (list items))
;        (else (append (deep-reverse (cdr items)) (deep-reverse (car items))))))
;Value: (4 3 2 1)

(define (deep-reverse items)
  (if (null? items)
      items
      (append (deep-reverse (cdr items)) 
              (if (pair? (car items))
                  (list (deep-reverse (car items)))
                  (list (car items))))))
;Value: ((4 3) (2 1))


;;; Exercise 2.28
(define (fringe root)
  (cond ((null? root) root)
        ((not (pair? root)) (list root))
        (else (append (fringe (car root)) (fringe (cdr root))))))


;;; Exercise 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;;; 2.29 a
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

;;; 2.29 b
(define (total-weight mobile)
  (define (branch-weight branch)
      (if (pair? (branch-structure branch))
          (total-weight (branch-structure branch))
          (branch-structure branch)))
  (+ (branch-weight (left-branch mobile)) 
     (branch-weight (right-branch mobile))
  ))

;;; 2.29 c
(define (branch-weight branch)
  (if (pair? (branch-structure branch))
      (total-weight (branch-structure branch))
      (branch-structure branch)))

(define (balanced mobile)
  (if (not (pair? mobile))
      #t
      (and (= (* (branch-length (left-branch mobile))
                 (branch-weight (left-branch mobile)))
              (* (branch-length (right-branch mobile))
                 (branch-weight (right-branch mobile))))
           (balanced (branch-structure (left-branch mobile)))
           (balanced (branch-structure (right-branch mobile)))
      )
  ))


;;; 2.29 d
; Change the definition, we only need to change the selector.
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (scale-tree tree factor)
  (cond ((null? tree) `())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
    (if (pair? sub-tree)
      (scale-tree sub-tree factor)
      (* sub-tree factor))) tree))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

;;; Exercise 2.30

(define (square-tree tree)
  (cond ((null? tree) `())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree)) 
                    (square-tree (cdr tree))))))

(square-tree 
   (list 1 
         (list 2 
         (list 3 4) 5)
         (list 6 7)))

(define (square-tree tree)
  (map (lambda (sub-tree) 
          (if (pair? sub-tree)
            (square-tree sub-tree)
            (square sub-tree))) tree))

(square-tree 
   (list 1 
         (list 2 
         (list 3 4) 5)
         (list 6 7)))

;;; Exercise 2.31
(define (tree-map func tree)
  (map (lambda (sub-tree) 
          (if (pair? sub-tree)
            (tree-map func sub-tree)
            (func sub-tree))) tree))

(define (square-tree tree)
  (tree-map square tree))

(square-tree 
   (list 1 
         (list 2 
         (list 3 4) 5)
         (list 6 7)))

;;; Exercise 2.32

(define (subsets s)
  (if (null? s)
      (list `())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (sub-set) (cons (car s) sub-set)) rest)))))

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 
         (if (odd? tree)
             (square tree)
             0))
        (else (+ (sum-odd-squares (car tree)) 
                 (sum-odd-squares (cdr tree))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        `()
        (let ((f (fib k)))
            (if (even? f)
                (cons f (next (+ k 1)))
                (next (+ k 1))))))
  (next 0))

(define (filter predicate sequence)
  (cond ((null? sequence) `())
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      `()
      (cons low (enumerate-interval (+ low 1) high))))

; fringe from Ex 2.28

(define (enumerate-tree tree)
  (cond ((null? tree) `())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree)) 
                      (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square (filter odd?
                                  (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
              `()
              (filter even?
                      (map fib 
                           (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
              `()
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))

;;; Exercise 2.33
(define (map* p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) `() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

;;; Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

;;; Exercise 2.35
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x) 1) (enumerate-tree t))))

;;; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      `()
      (cons (accumulate op init (map (lambda (seq) (car seq)) seqs))
            (accumulate-n op init (map (lambda (seq) (cdr seq)) seqs)))))

;;; Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (mi) (dot-product mi v)) m))

(define (transpose mat)
  (accumulate-n cons `() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
      (map (lambda (row) (matrix-*-vector cols row)) m)))

;;; Exercise 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (accumulate op initial sequence))
; Associative property, like '+', '*', 'set union', 'set intersection'.

;;; Exercise 2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) `() sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) `() sequence))

(define (flatmap proc seq)
  (accumulate append `() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

