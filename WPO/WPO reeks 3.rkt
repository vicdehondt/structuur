; 3.1
(define (1- x) (- x 1))

(define (1+ x) (+ 1 x))

; 3.1.1
(define (rec-add a b)
  (if (= b 0)
      a
      (1+ (rec-add a (1- b)))))

; 3.1.2
(define (iter-add a b)
  (if (zero? b)
      a
      (iter-add (1+ a) (1- b))))

; 3.2
(define (rec-multiply a b)
  (if (zero? b)
      0
      (+ a (rec-multiply a (- b 1)))))

(define (iter-multiply a b)
  (define (iter result counter)
    (if (zero? counter)
        result
        (iter (+ result a) (- counter 1))))
  (iter 0 b))

(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

(define (square x)
  (* x x))

; 3.2.1
(define (rec-fast-multiply a b)
  (cond
    ((zero? b) 0)
    ((even? b) (rec-fast-multiply (double a) (halve b)))
    (else (+ a (rec-fast-multiply a (- b 1))))))

(define (iter-fast-multiply a b)
  (define (iter a b acc)
    (cond
      ((zero? b) acc)
      ((even? b) (iter (double a) (halve b) acc))
      (else (iter a (- b 1) (+ acc a)))))
  (iter a b 0))

; 3.3.1
(define (calc-e n)
  (define (iter counter result prev-fac)
    (if (> counter n)
        result
        (iter (+ counter 1) (+ result (/ 1 (* counter prev-fac))) (* counter prev-fac))))
  (iter 1 1 1))

; 3.5
(define (my-odd? x)
  (if (zero? x)
      #f
      (my-even? (- x 1))))

(define (my-even? x)
  (if (zero? x)
      #t
      (my-odd? (- x 1))))

; 3.6.1
(define (weird x)
  (cond ((= x 1) 1)
        ((even? x) (weird (/ x 2)))
        (else (weird (+ (* 3 x) 1)))))

; 3.6.2
(define (depth-weird x)
  (define (depth x counter)
    (cond ((= x 1) counter)
          ((even? x) (depth (/ x 2) (+ counter 1)))
          (else (depth (+ (* 3 x) 1) (+ counter 1)))))
  (depth x 0))

; 3.7.2
(define (display-as-binary n)
  (if (<= n 1)
      (if (even? n)
          (display "0")
          (display "1"))
      (if (even? n)
          (begin (display-as-binary (quotient n 2))
             (display "0"))
          (begin (display-as-binary (quotient n 2))
             (display "1")))))

; 3.9.1
(define (display-n x n)
  (if (zero? n)
      (display "")
      (begin
        (display x)
        (display-n x (- n 1)))))

; 3.9.2
(define (parasol n)
  (define (1-star x)
    (begin
      (display-n " " x)
      (display-n "*" 1)
      (newline)))
  (define (blank-width x)
    (/ (- (* 2 n) 2) 2))
  (define (counter count star)
    (if (= count (+ n 1))
      (begin
        (1-star (blank-width n))
        (1-star (blank-width n))
        (1-star (blank-width n)))
      (begin
        (display-n " " (- (blank-width n) (- count 1)))
        (display-n "*" star)
        (display "*")
        (display-n "*" star)
        (newline)
        (counter (+ count 1) (+ star 1)))))
  (counter 1 0))