(#%require racket/trace)

; 6.1.1
(define (x a) (car a))
(define (y a) (cdr a))

(define (make-punt x y)
  (cons x y))

; 6.1.2
(define start-punt car)
(define end-punt cdr)

(define (make-segment start einde)
  (cons start einde))

; 6.1.3
(define (middelpunt segment)
  (make-punt (/ (+ (x (start-punt segment)) (x (end-punt segment))) 2) (/ (+ (y (start-punt segment)) (y (end-punt segment))) 2)))