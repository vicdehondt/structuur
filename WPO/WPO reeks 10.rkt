(#%require "streams.rkt")

; Oef 10.1
(define (fac n)
  (if (= n 0)
      1
      (* n (fac (- n 1)))))
 
(define (calc-e n)
  (accumulate + 0 (map-stream (lambda (x) (/ 1 (fac x)))
                              (enumerate-interval 0 n))))

(define (sinus x n)
  (define (calc-term t)
    (if (odd? (/ (- t 1) 2))
        (* -1 (/ (expt x t) (fac t)))
        (/ (expt x t) (fac t))))
  (accumulate +
              0
              (map-stream calc-term
                          (streamfilter odd?
                                        (enumerate-interval 0 (* n 2))))))



(define (exp x n)
  (if (zero? n)
      1
      (* x (exp x (- n 1)))))

; Oef 10.2
(define (sum-odd-squares str)
  (accumulate + 0 (map-stream (lambda (n) (* n n)) (streamfilter odd? str))))

; Oef 10.3
(define (odd-sum-tripples max)
  (map-stream (lambda (x) (let ((l (car x))
                                (r (cdr x)))
                            (list l r (+ l r))))
              (let ((s (streamfilter odd? (enumerate-interval 0 (- max 1)))))
                (pairs s s))))

; Oef 10.4
(define (fac n)
  (display "->")(display n)(newline)
  (if (= n 0)
      1
      (* n (fac (- n 1)))))

; Oef 10.5
(define (show x)
  (display x)(newline)
  x)

; Oef 10.7
(define integers (cons-stream 1 (map-stream (lambda (x) (+ x 1)) integers)))

; Oef 10.9
(define (triplets)
  (streamfilter (lambda (x) (> (+ (car x) (cadr x)) (caddr x))) (cartesian-product integers integers integers)))



; Oef 10.12
(define (cut s)
  (define (split result s)
    (cond
      ((empty-stream? s) (cons result the-empty-stream))
      ((eq? (head result) (head s)) (split (cons-stream (head s) result) (tail s)))
      (else (cons result s))))
  (if (empty-stream? s)
      the-empty-stream
      (let ((result (split (cons-stream (head s) the-empty-stream) (tail s))))
        (cons-stream (car result)
                     (cut (cdr result))))))

(define (merge s1 s2)
  (cond
    ((empty-stream? s1) s2)
    ((empty-stream? s2) s1)
    ((> (head s1) (head s2))
     (merge s2 s1))
    (else (cons-stream (head s1) (merge (tail s1) s2)))))

(define (merge-n s)
  (define (loop s1 s2)
    (cond
      ((empty-stream? s2) s1)
      ((empty-stream? (tail s2)) (merge s1 (head s2)))
      (else (merge (merge s1 (head s2)) (head (tail s2))))))
  (loop (head s) (tail s)))

(define (pretpark-traffiek s)
  (map-stream (lambda (x) (cons (head x)
                                (accumulate + 0 (map-stream (lambda (y) 1) x))))
              (cut (merge-n s))))

; Oef 10.16.1


; Oef 10.16.2


; Oef 10.16.3
#|
(define (daggemiddelden stream)
  (define (aux stream i)
    (cond
      ((empty-stream? stream) the-empty-stream)
      ((= i 12) ())
      (else (let ((temp (aux (tail stream) (+ i 1))))
              (cons (/ (accumulate + 0 (cons-stream (head stream) (car temp)))) (cdr temp))))))
  (if (empty-stream? stream)
      the-empty-stream
      (let ((temp (aux stream)))
        (cons-stream (car temp) (daggemiddelden (cdr temp))))))|#














; Oef voor examenvoorbereiding:





(define integers (cons-stream 1 (map-stream (lambda (x) (+ x 1)) integers)))

(define (integers-special stream)
  (streamfilter (lambda (x) (and (not (= (modulo x 2) 0)) (not (= (modulo x 3) 0)) (not (= (modulo x 5) 0)))) stream))

(define (triplets)
  (streamfilter (lambda (x) (> (+ (car x) (cadr x)) (caddr x))) (cartesian-product integers integers integers)))

(define (accumulate-n op ne streams)
  (if (null? (head streams))
      '()
      (cons-stream (accumulate op ne (map-stream head streams)) (accumulate-n op ne (map-stream tail streams)))))
(define matrix
  (cons-stream (enumerate-interval 1 3)
               (cons-stream (enumerate-interval 4 6)
                            (cons-stream (enumerate-interval 7 9)
                                         (cons-stream (enumerate-interval 10 12)
                                                      the-empty-stream)))))

(define (print-m matrix)
  (display "[")
  (print-stream (head matrix)) (newline)
  (stream-for-each (lambda (x) (display " ")
                     (print-stream x) (newline))
                   (tail matrix))
  (display "]\n"))

(define (transpose m)
  (if (empty-stream? (head m))
      the-empty-stream
      (cons-stream (map-stream head m) (transpose (map-stream tail m)))))

(define (cut stream)
  (if (empty-stream? stream)
      the-empty-stream
      (cons-stream (streamfilter (let ((int (head stream))) (lambda (x) (= int x))) stream)
                   (cut (streamfilter (let ((int (head stream))) (lambda (x) (not (= int x)))) stream)))))

(define (merge str1 str2)
  (cond
    ((empty-stream? str1) str2)
    ((empty-stream? str2) str1)
    ((<= (head str1) (head str2)) (cons-stream (head str1) (merge (tail str1) str2)))
    (else (cons-stream (head str2) (merge str1 (tail str2))))))

(define (merge-n n-stream)
  (if (empty-stream? (tail (tail n-stream)))
      (merge (head n-stream) (head (tail n-stream)))
      (merge-n (cons-stream (merge (head n-stream) (head (tail n-stream))) (tail (tail n-stream))))))

(define (raam data n)
  (define (aux stream i)
    (cond
      ((empty-stream? stream) the-empty-stream)
      ((= i n) (cons-stream (head stream) the-empty-stream))
      (else (cons-stream (head stream) (aux (tail stream) (+ i 1))))))
  (if (empty-stream? data)
      the-empty-stream
      (cons-stream (aux data 1) (raam (tail data) n))))

(define (test-n stream pred n)
  (define (pred-test sub-stream i)
    (cond
      ((or (and (<= i n) (empty-stream? sub-stream)) (not (pred (head sub-stream)))) #f)
      ((and (= i n) (pred (head sub-stream))) #t)
      (else (pred-test (tail sub-stream) (+ i 1)))))
  (if (empty-stream? stream)
      the-empty-stream
      (cons-stream (pred-test (head stream) 1) (test-n (tail stream) pred n))))





