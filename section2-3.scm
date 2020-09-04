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

;;; Exercise 2.60 Allow duplicates.
; O(n) complexity.
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; O(1) complexity.
(define (adjoin-set x set) (cons x set))

; O(n^2) complexity.
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) `())
        ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; O(n) complexity.
(define (union-set set1 set2) (append set1 set2))

;;; Representing sets as ordered lists.
; O(n) complexity, but a factor of 2 in number of steps over the unordered one.
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

; O(n) complexity.
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      `()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2) 
                (cons x1 
                      (intersection-set (cdr set1)
                                        (cdr set2))))
              ((< x1 x2) 
                (intersection-set (cdr set1) set2))
              ((< x2 x1) 
                (intersection-set set1 (cdr set2)))))))

;;; Exercise 2.61
; O(n) complexity, but requires on average about half as many steps as unordered one.
(define (adjoin-set x set)
  (cond ((null? set) (cons x set))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 1 '())
;Value: (1)

(adjoin-set 1 '(1 2))
;Value: (1 2)

(adjoin-set 3 '(1 2 3))
;Value: (1 2 3)

(adjoin-set 2 '(1 3 4))
;Value: (1 2 3 4)

(adjoin-set 4 '(1 2 3))
;Value: (1 2 3 4)

;;; Exercise 2.62
; O(n) complexity.
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                  ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                  ((< x2 x1) (cons x2 (union-set set1 (cdr set2)))))))))

(union-set '() '())
;Value: ()

(union-set '(1) '())
;Value: (1)

(union-set '() '(2))
;Value: (2)

(union-set '(1 2) '(3 4))
;Value: (1 3 2 4)

(union-set '(1 2) '(1 2))
;Value: (1 2)

(union-set '(1 2 3 4) '(2 3))
;Value: (1 2 3 4)

(union-set '(2 3) '(1 2 3 4))
;Value: (1 2 3 4)

;;; Representing sets as binary trees.
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

; O(log n) complexity.
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))

; O(log n) complexity.
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x `() `()))
        ((= x (entry set)) set)
        ((< x (entry set)) 
          (make-tree (entry set)
                     (adjoin-set x (left-branch set))
                     (right-branch set)))
        ((> x (entry set)) 
          (make-tree (entry set)
                     (left-branch set)
                     (adjoin-set x (right-branch set))))))

;;; Exercise 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      `()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree) (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                            result-list)))))
  (copy-to-list tree `()))

; a. Do the two procudures produce the same result for every tree?
; Yes, they are both in-order traverse of the tree.
(define tree1 (make-tree 7 (make-tree 3 (make-tree 1 '() '())
                                        (make-tree 5 '() '())) 
                           (make-tree 9 '() 
                                        (make-tree 11 '() '()))))

(define tree2 (make-tree 3 (make-tree 1 '() '())
                           (make-tree 7 (make-tree 5 '() '())
                                        (make-tree 9 '() (make-tree 11 '() '())))))

(define tree3 (make-tree 5 (make-tree 3 (make-tree 1 '() '())
                                        '())
                           (make-tree 9 (make-tree 7 '() '())
                                        (make-tree 11 '() '()))))

(tree->list-1 tree1)
;Value: (1 3 5 7 9 11)

(tree->list-2 tree1)
;Value: (1 3 5 7 9 11)

(tree->list-1 tree2)
;Value: (1 3 5 7 9 11)

(tree->list-2 tree2)
;Value: (1 3 5 7 9 11)

(tree->list-1 tree3)
;Value: (1 3 5 7 9 11)

(tree->list-2 tree3)
;Value: (1 3 5 7 9 11)

; b. Do the two procedures have the same order of growth?
; No, 
; tree->list-1 T(n) = 2 * T(n/2) + O(n) ==> O(n log n)
; tree->list-2 T(n) = 2 * T(n/2) + O(1) ==> O(n)

;;; Exercise 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons `() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

; a. how partial-tree works? Draw the tree produced by list->tree for the list (1 3 5 7 9 11)
;              5
;            /   \
;           1     9
;            \   / \
;             3 7  11
(list->tree '(1 3 5 7 9 11))
;Value: (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
; partial-tree divide the list to two halves and one this-entry list[(n - 1)/2]
; recursively construct the left-tree and right-tree by consuming the elements.
; Eventually, the elements will be an empty list.

; b. Time complexity?
; T(n) = 2 * T(n/2) + O(1) ==> O(n)

;;; Exercise 2.65
; rename union-set (ordered list implementation) as merge-ordered-list
(define (merge-ordered-list list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else (let ((x1 (car list1)) (x2 (car list2)))
            (cond ((= x1 x2) (cons x1 (merge-ordered-list (cdr list1) (cdr list2))))
                  ((< x1 x2) (cons x1 (merge-ordered-list (cdr list1) list2)))
                  ((< x2 x1) (cons x2 (merge-ordered-list list1 (cdr list2)))))))))

; rename intersection-set (ordered list implementation) as diff-ordered-list
(define (diff-ordered-list list1 list2)
  (if (or (null? list1) (null? list2))
      `()
      (let ((x1 (car list1)) (x2 (car list2)))
        (cond ((= x1 x2) 
                (cons x1 
                      (diff-ordered-list (cdr list1)
                                        (cdr list2))))
              ((< x1 x2) 
                (diff-ordered-list (cdr list1) list2))
              ((< x2 x1) 
                (diff-ordered-list list1 (cdr list2)))))))

; O(n) complexity.
(define (tree->list elements)
  (tree->list-2 elements))

; O(n) complexity.
(define (union-set set1 set2)
  (let ((list1 (tree->list set1))   ; O(n)
        (list2 (tree->list set2)))  ; O(n)
    (list->tree (merge-ordered-list list1 list2))))  ; O(n) + O(n)

; O(n) complexity.
(define (intersection-set set1 set2)
  (let ((list1 (tree->list set1))   ; O(n)
        (list2 (tree->list set2)))  ; O(n)
    (list->tree (diff-ordered-list list1 list2))))   ; O(n) + O(n)

(define (make-record key value) (cons key value))

(define (key record) (car record))

(define (value record) (cdr record))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (car set-of-records))) 
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))
