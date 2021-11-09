(#%require racket/trace)

(define (x a) (car a))
(define (y a) (cdr a))

(define (make-punt x y)
  (cons x y))

(define start-punt car)
(define end-punt cdr)

(define (make-segment start einde)
  (cons start einde))

(define (middelpunt segment)
  (make-punt (/ (+ (x (start-punt segment)) (x (end-punt segment))) 2) (/ (+ (y (start-punt segment)) (y (end-punt segment))) 2)))