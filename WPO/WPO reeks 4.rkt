; 4.7
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

; 4.7.1
(define (product factor a next b)
  (if (> a b)
      1
      (* (factor a) (product factor (next a) next b))))

; 4.7.2
(define (iter-product factor a next b)
  (define (iter i result)
    (if (> i b)
        result
        (iter (next i) (* (factor i) result))))
  (iter a 1))

(define (id x) x)

; 4.7.3
(define (factorial n)
  (product id 1 (lambda (x) (+ x 1)) n))

; 4.9.1
(define (accumulate combiner null-value term a next b)
  (define (iter i result)
  (if (<= i b)
      (iter (next i) (combiner (term i) result))
      result))
  (iter a null-value))

; 4.9.2
(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product factor a next b)
  (accumulate * 1 factor a next b))

; 4.9.3
(define (add a b)
  (accumulate + a (lambda (i) 1) 1 (lambda (i) (+ i 1)) b))

(define (multiply a b)
  (accumulate + 0 (lambda (x) a) 1 (lambda (x) (+ x 1)) b))

; 4.10.1
(define (filtered-accumulate combiner filter? null-value term a next b)
  (define (iter a res)
    (if (<= a b)
        (if (filter? (term a))
            (iter (next a) (combiner (term a) res))
            (iter (next a) res))
        res))
  (iter a null-value))

; 4.10.2
(define (product-gcd n)
  (define (incr x) (+ x 1))
  (define (id x) x)
  (define (filter? a)
    (= (gcd a n) 1))
  (filtered-accumulate * filter? 1 id 1 incr n))

; 4.14
(define ((compose f g) x)
  (f (g x)))

;assistent:
;(define (compose f g)
;  (lambda (x)
;    (f (g x))))

; 4.15
(define (do-n f n)
  (define (iter counter)
    (if (= counter n)
        (display "")
        (begin
          (f)
          (iter (+ counter 1)))))
  (iter 0))