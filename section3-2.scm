(define (square x)
  (* x x))

(define square
  (lambda (x) (* x x)))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

;;; Exercise 3.9
; from section 1.2.1
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
;               _________________________
;               |                       |
;global ______\ | factorial --|         |
;env          / |             |         |
;               |_____________|_________|
;                             |   ^
;                             |   |
;                             v   |
;                            O O--|
;                  parameter: n
;                  body: (if (= n 1)
;                          1
;                          (* n (factorial (- n 1))))

(factorial 6)
;               _________________________________________________
;               |                                               |
;global ______\ |                                               |
;env          / |                                               |
;               |_______________________________________________|
;                  ^       ^       ^       ^       ^       ^
;                  |       |       |       |       |       |
;              E1->n:6 E2->n:5 E3->n:4 E4->n:3 E5->n:2 E6->n:1
;   (* n (factorial (- n 1)))                              1

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
;               _________________________________________________
;               |                                               |
;global ______\ | fact-iter -------------------------|          |
;env          / | factorial --|                      |          |
;               |_____________|______________________|__________|
;                             |   ^                  |   ^
;                             |   |                  |   |
;                             v   |                  v   |
;                            O O--|                 O O--|
;                parameter: n             parameter: product counter max-count
;                body: (fact-iter 1 1 n)) body:   (if (> counter max-count)
;                                                   product
;                                                   (fact-iter (* counter product)
;                                                              (+ counter 1)
;                                                              max-count)))

(factorial 6)
;               _____________________________________________________________
;               |                                                           |
;global ______\ |                                                           |
;env          / |                                                           |
;               |___________________________________________________________|
;                 ^       ^       ^       ^       ^       ^       ^       ^
;                 |       |       |       |       |       |       |       |
;             E1->n:6 E2->    E3->    E4->    E5->    E6->    E7->    E8->
;                   product:1       1       2       6       24      120     720
;                   counter:1       2       3       4       5       6       7
;                   max-count:6
;    (fact-iter (* counter product) (+ counter 1) max-count)                720                             

;;; Exercise 3.10
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
      (lambda (amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) 
                   balance)
            "Insufficient funds"))))

; (let ((<var> <exp>)) <body>)
; ((lambda (<var>) <body>) <exp>)

;               _____________________________________________________________
;               |                                                           |
;global ______\ | make-withdraw--|                                          |
;env          / |                |                                          |
;               |________________|__________________________________________|
;                                |   ^
;                                |   |
;                                v   |
;                               O O--|
;                       parameter: initial-amount
;                       body: ((lambda (balance) 
;                               (lambda (amount)
;                                 (if (>= balance amount)
;                                     (begin (set! balance (- balance amount)) 
;                                            balance)
;                                     "Insufficient funds"))) 
;                               initial-amount)


;               _____________________________________________________________
;               | make-withdraw                                             |
;global ______\ | W2 ---------------------------|                           |
;env          / | W1 --|                        |                           |
;               |______|________________________|___________________________|
;                      |            ^           |            ^
;                      |            |           |            |
;                      |  E1 --> balance: 100   |  E2 --> balance: 100
;                      |            |           |            |
;                      v            |           v            |
;                     O O-----------|          O O-----------|
;                     |                        |
;                     v                        v
;           one copy of the code       the other copy of the code
;
; The body is not shared.

;;; Exercise 3.11
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unkown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

;               _____________________________________________________________
;               |                                                           |
;global ______\ | make-account --|                                          |
;env          / |                |                                          |
;               |________________|__________________________________________|
;                                |   ^
;                                |   |
;                                v   |
;                               O O--|
;                       parameter: balance
;                       body: (define (withdraw amount)
;                               (if (>= balance amount)
;                                   (begin (set! balance (- balance amount))
;                                          balance)
;                                   "Insufficient funds"))
;                             (define (deposit amount)
;                               (set! balance (+ balance amount))
;                               balance)
;                             (define (dispatch m)
;                               (cond ((eq? m 'withdraw) withdraw)
;                                     ((eq? m 'deposit) deposit)
;                                     (else (error "Unkown request -- MAKE-ACCOUNT"
;                                                  m))))
;                             dispatch)

; Each frame of the account will contains their own balance as well as 
; 'withdraw', 'deposit', 'dispatch' procedures.

;               _____________________________________________________________
;               | make-account -- ...                                       |
;global ______\ | acc2 ----------------------|                              |
;env          / | acc --|                    |                              |
;               |_______|____________________|______________________________|
;                        __________________  |       __________________
;                        | balance: 1000   | |       | balance: 5000   |
;                 E1 --> | withdraw -->    | |E2 --> | withdraw -->    |
;                        | deposit  -->    | |       | deposit  -->    |
;                        | dispatch -->    | |       | dispatch -->    |
;                        |_________________| |       |_________________|
;                                            v                ^
;                                           O O---------------|
;
; The only part of environment structure are shared between acc and acc2 is the
; global environment, 'make-account' procedures.
