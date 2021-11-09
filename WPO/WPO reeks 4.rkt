(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (product factor a next b)
  (if (> a b)
      1
      (* (factor a) (product factor (next a) next b))))

;(define (iter-product factor a next b)
;  (define (iter factor a next b count result)
;    (if (< count 0)
;        result
;        (iter factor (next a) next b (- count 1) (* (factor a) result))))
;  (iter factor a next b (- b a) 1))

; assistent:
(define (iter-product factor a next b)
  (define (iter i result)
    (if (> i b)
        result
        (iter (next i) (* (factor i) result))))
  (iter a 1))

(define (id x) x)

(define (factorial n)
  (product id 1 (lambda (x) (+ x 1)) n))

(define (accumulate combiner null-value term a next b)
  (define (iter i result)
  (if (<= i b)
      (iter (next i) (combiner (term i) result))
      result))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product factor a next b)
  (accumulate * 1 factor a next b))

(define (add a b)
  (accumulate + a (lambda (i) 1) 1 (lambda (i) (+ i 1)) b))

(define (multiply a b)
  (accumulate + 0 (lambda (x) a) 1 (lambda (x) (+ x 1)) b))

(define (filtered-accumulate combiner filter? null-value term a next b)
  (define (iter a res)
    (if (<= a b)
        (if (filter? (term a))
            (iter (next a) (combiner (term a) res))
            (iter (next a) res))
        res))
  (iter a null-value))

(define ((compose f g) x)
  (f (g x)))

;assistent:
;(define (compose f g)
;  (lambda (x)
;    (f (g x))))

(define (do-n f n)
  (define (iter counter)
    (if (= counter n)
        (display "")
        (begin
          (f)
          (iter (+ counter 1)))))
  (iter 0))