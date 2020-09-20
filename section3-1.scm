(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(define new-withdraw
  (let ((balance 100))
      (lambda (amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

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
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

;;; Exercise 3.1
(define (make-accumulator sum)
  (lambda (n)
    (set! sum (+ sum n))
    sum))

;;; Exercise 3.2
(define (make-monitored f)
  (define counter 0)
  (define (mf m)
    (cond ((eq? m 'how-many-calls?) counter)
          ((eq? m 'reset-count) (set! counter 0))
          (else (begin (set! counter (+ counter 1))
                       (f m)))))
  mf)

;;; Exercise 3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
                balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (access password)
    ;;; Exercise 3.4
    (let ((retries 0))
      (lambda (pw m)
        (if (and (eq? password pw) (< retries 7))
          (begin (set! retries 0)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  ((eq? m 'addtional-access) access)
                  (else (error "Unknown request" m))))
          (lambda (n)
            (if (< retries 7)
                (begin (set! retries (+ retries 1))
                       "Incorrect password")
                "call-the-cops"))))))
  (access password))

(define rand (lambda () (random 1000)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;;; Exercise 3.5
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define radius 3)
(define x0 5)
(define y0 7)
(define x1 2)
(define x2 8)
(define y1 4)
(define y2 10)

(define (p)
  (let ((x (random-in-range x1 x2)))
    (let ((y (random-in-range y1 y2)))
      (> (square radius) (+ (square (- x x0)) (square (- y y0)))))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (* (monte-carlo trials p) (abs (* (- x2 x1) (- y2 y1)))))

;;; Exercise 3.6
(define (rand-update x) (+ x 1))

(define rand-new 
  (let ((rand-init 0))
    (lambda (m)
      (cond ((eq? m 'reset) (lambda (new-value) (set! rand-init new-value)))
            ((eq? m 'generate) (set! rand-init (rand-update rand-init)))))
  ))

;;; Exercise 3.7
(define (make-joint acc password new-password)
  ((acc password 'addtional-access) new-password))

;;; Exercise 3.8
(define f (let ((v 1))
  (lambda (n) (set! v (* v n)) v)))
(+ (f 0) (f 1))
(+ (f 1) (f 0))
