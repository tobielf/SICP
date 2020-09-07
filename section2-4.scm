(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

; A representation easier for add/sub-complex, convert to magnitude+angle for mul/div-complex
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle-rectangular z)
  (atan (imag-part z) (real-part z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

; A representation easier for mul/div-complex, convert to real+imagine for add/sub-complex
(define (real-part-polar z)
  (* (magnitude z) (cos (angle z))))
(define (imag-part-polar z)
  (* (magnitude z) (sin (angle z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (real-part z)
  (cond ((rectangular? z) 
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z) 
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) 
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
      (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
      (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) (atan y x)))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
      (lambda (x y) (tag make-from-real-imag x y)))
  (put 'make-from-mag-ang 'polar
      (lambda (r a) (tag make-from-mag-ang r a)))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if (proc)
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular x y)))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar r a)))

;;; Exercise 2.73
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) 
                                           var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;a. We can't assimilate number? and variable? into the data-directed dispatch
;   since they are generic operation for all of the 'operator(s)

;b.
(define (install-sum-package)
  ;; internal procedures
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (cons a1 a2))))
  (define (addend s) (car s))
  (define (augend s) (cdr s))
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag '+ x))
  (put 'deriv '+ 
      (lambda (exp var) (tag (deriv-sum exp var))))
  (put 'make-sum '+
      (lambda (a1 a2) (tag (make-sum a1 a2))))
  'done)

(define (install-product-package)
  ;; internal procedures
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (cons m1 m2))))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cdr p))
  (define (deriv-product exp var)
    ((get 'make-sum '+)
      (make-product (multiplier exp)
                    (deriv (multiplicand exp) var))
      (make-product (deriv (multiplier exp) var)
                    (multiplicand exp))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag '* x))
  (put 'deriv '*
      (lambda (exp var) (tag (deriv-product exp var))))
  (put 'make-product '*
      (lambda (m1 m2) (tag (make-product m1 m2))))
  'done)

;c.
(define (install-exponent-package)
  ;; internal procedures
  (define (make-exponentiation b exp)
    (cond ((=number? exp 0) 1)
          ((=number? exp 1) b)
          (else (cons b exp))))
  (define (base e) (car e))
  (define (exponent e) (cdr e))
  (define (deriv-exponent exp var)
    ((get 'make-product '*) (exponent exp)
                            ((get 'make-product '*) (make-exponentiation (base exp) (- (exponent exp) 1))
                                                    (deriv (base exp) var))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag '** x))
  (put 'deriv '**
      (lambda (exp var) (tag (deriv-exponent exp var))))
  (put 'make-exponentiation '**
      (lambda (b exp) (tag (make-exponentiation b exp))))
  'done)

;d. if we change to get ((get 'deriv (operator exp)) (operands exp) var)))
;   we only need to change the corresponding (put '* 'deriv)

;;; Exercise 2.74, operation-type tables
;                          Divisions
;                Div1    |    Div2    |    Div3
;===================================================
;o
;p  get-record |
;e ------------
;r  get-salary |
;a ------------
;t  find-empl- |
;i  oyee-record|
;o
;o
;s

(define div1-file (attach-tag 'div1 (list (cons 'John (list '100-blvd '$15000/mo)))))
(define (install-div1-file-format)
  (define (make-employee name record)
    (cons name record))
  (define (name employee)
    (car employee))
  (define (record employee)
    (cdr employee))
  (define (make-record address salary)
    (list address salary))
  (define (address record)
    (car record))
  (define (salary record)
    (cadr record))
  (define (get-record name file)
    (cond ((null? file) #f)
          ((equal? name (name (car file))) (record (car file)))
          (else (get-record name (cdr file)))))
  (define (add-employee employee file)
    (cons employee file))
  (define (tag x) (attach-tag 'div1 x))
  (put 'get-record 'div1 
      (lambda (name file) (tag (get-record name file))))
  (put 'get-salary 'div1
      (lambda (record) (tag (salary record))))
  'done)
(define div2-file (attach-tag 'div2 (list (list 'King (cons '200-ave '$12000/mo)))))

;a.
(define (get-record employee-name file)
  ((get 'get-record (type-tag file)) employee-name (contents file)))

;b.
(define (get-salary employee-record)
  ((get 'get-salary (type-tag employee-record)) (contents employee-record)))

;c.
(define (find-employee-record employee-name files)
  (map (lambda (file) (get-record employee-name file)) files))

;d. A new division adding to the central system, we only need to wrap all operations
;   within an "install" procedure, assign a new data-tag to it, and register all 
;   operations with new data-tag to the table.
;   We don't need to change the central system.

