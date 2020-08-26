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

(define (permutations s)
  (if (null? s)
      (list `())
      (flatmap (lambda (x)
                  (map (lambda (p) (cons x p))
                       (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;;; Exercise 2.40
(define (unique-pairs n)
  (flatmap (lambda (i) 
             (map (lambda (j) (list i j)) 
                  (enumerate-interval 1 (- i 1)))) 
           (enumerate-interval 2 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

;;; Exercise 2.41
(define (unique-triples n)
  (flatmap (lambda (k)
             (map (lambda (pairs) (cons k pairs))
             (unique-pairs (- k 1))))
           (enumerate-interval 3 n)))

(define (sum-up-to? s)
  (lambda (pair) (= s (accumulate + 0 pair))))

(define (triples-sum-up-to s n)
  (filter (sum-up-to? s) (unique-triples n)))

;;; Exercise 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                      (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

(define empty-board `())

(define (safe? k positions)
  (define (safe-row k positions new-row)
    (if (= k 0)
        #t
        (and (not (= (abs (- (car (car positions)) (car new-row))) (abs (- (cadr (car positions)) (cadr new-row))))) ; check diagonal
             (not (= (car (car positions)) (car new-row))) ; check row
             (safe-row (- k 1) (cdr positions) new-row))))
  (safe-row (- k 1) (cdr positions) (car positions)))

;;; Exercise 2.43
;  (n - 1)! T times slower, since the recursive call happens multiple times.
;(flatmap
;  (lambda (new-row)
;    (map (lambda (rest-of-queens)
;            (adjoin-position new-row k rest-of-queens))
;         (queen-cols (- k 1))))
;  (enumerate-interval 1 board-size))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;;; Exercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (square-of-four tl tr bl br)
  (lambda (painter) 
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;;; Exercise 2.45
(define (split op-to-origi op-to-small)
  (define (func painter n)
    (if (= n 0)
        painter
        (let ((smaller (func painter (- n 1))))
          (op-to-origi painter (op-to-small smaller smaller)))))
  func)

(define right-split (split beside below))

(define up-split (split below beside))

(define (frame-coord-map frame)
  (lambda (v) 
    (add-vect
      (origin-frame frame)
    (add-vect (scale-vect (xcor-vect v)
                          (edge1-frame frame))
              (scale-vect (ycor-vect v)
                          (edge2-frame frame))))))

;;; Exercise 2.46
(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cadr v))

(define (add-vect u v)
  (make-vect (+ (xcor-vect u) (xcor-vect v))
             (+ (ycor-vect u) (ycor-vect v))))

(define (sub-vect u v)
  (make-vect (- (xcor-vect u) (xcor-vect v))
             (- (ycor-vect u) (ycor-vect v))))

(define (scale-vect v s)
  (make-vect (* (xcor-vect v) s) 
             (* (ycor-vect v) s)))

;;; Exercise 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cadr (cdr frame)))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cdr (cdr frame)))

(define (segments->painter segment-list)
  (lambda (frame) 
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
    segment-list)))

;;; Exercise 2.48
(define (make-segment start end)
  (list start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cadr segment))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame) 
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
          (make-frame new-origin
                      (sub-vect (m corner1) new-origin)
                      (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2

;;; Exercise 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left (transform-painter painter1
                                         (make-vect 0.0 0.0)
                                         split-point
                                         (make-vect 0.0 1.0)))
          (paint-right (transform-painter painter2
                                          split-point
                                          (make-vect 1.0 0.0)
                                          (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

;;; Exercise 2.51
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom (transform-painter painter1
                                        (make-vect 0.0 0.0)
                                        (make-vect 1.0 0.0)
                                        split-point))
          (paint-top (transform-painter painter2
                                        split-point
                                        (make-vect 1.0 0.5)
                                        (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))
