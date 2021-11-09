(#%require racket/trace)
;Reeksontwikkeling: Cosinus
(define (calc-cos x n)
  (define (iter term_count previous_exponent previous_nominator previous_denominator result)
    (define current_exponent (+ previous_exponent 2))
    (define current_nominator (* previous_nominator x x))
    (define current_denominator (* previous_denominator (+ previous_exponent 1) (+ previous_exponent 2)))
    (define (plus/minus question term1 term2) (if question
                                                  (+ term1 term2)
                                                  (- term1 term2)))
    (if (>= term_count n)
        result
        (iter (+ term_count 1)
              current_exponent
              current_nominator
              current_denominator
              (plus/minus (zero? (modulo current_exponent 4)) result
                          (/ current_nominator current_denominator)))))
  (trace iter)
  (if (< n 1)
      0
      (iter 1 0 1 1 1)))

;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;Binomiaalcoëfficiënten berekenen
(define (binom n k)
  (cond
    ((> k n) 0)
    ((< k 0) 0)
    ((= k n) 1)
    (else (+ (binom (- n 1) (- k 1)) (binom (- n 1) k)))))

;Dit is een boomrecursief proces. De eigenschap van dit proces is dat het aantal oproepen dat nodig is exponentieel groeit ten opzichte van de input.

;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;Driehoek
(define (pascal-driehoek n)
  (define (iter row column count)
    (if (> row n)
        (display "")
        (if (= row column)
            (if (< row n)
                (begin
                  (display (binom row column))
                  (newline)
                  (iter (+ row 1) 0 count))
                (begin
                  (display (binom row column))
                  (iter (+ row 1) 0 count)))
            (begin
              (display (binom row column))
              (display "\t")
              (iter row (+ column count) count)))))
  (if (< n 1)
      0
      (iter 0 0 1)))

;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;Tests

(define (test num actual expected)
  (if (equal? actual expected)
      (begin
        (display "Test")
        (display num)
        (display " passed")
        (newline))
      (begin
        (display "Test")
        (display num)
        (display " failed ")
        (display actual)
        (newline))))

(test 1 (list (calc-cos 0 10)
              (calc-cos (/ 3.1415 2) 10)
              (calc-cos 3.1415 10)
              (calc-cos 10 0))
      (list 1 4.6326794876592664e-5 -0.9999999992346591 0))



(test 2 (list (binom 0 0)
              (binom 1 1)
              (binom 2 1)
              (binom 3 2)
              (binom 6 3)
              (binom 0 5))
      (list 1 1 2 3 20 0))