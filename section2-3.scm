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
