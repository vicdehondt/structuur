(#%require (only racket error)
           racket/trace)

; Oef 9.3
(define (print-ring r)
  (define (aux l)
    (if (not (null? l))
        (cond ((eq? (cdr l) r) (display " ")
                               (display (car l))
                               (display "..."))
              (else (display " ")
                    (display (car l))
                    (aux (cdr l))))))
  (aux r))

(define (make-ring n)
  (define last '(0))

  (define (create-list n)
    (if (zero? n)
        last
        (cons n (create-list (- n 1)))))

  (let ((lst (create-list n)))
    (set-cdr! last lst)
    lst))

; Oef 9.4.1
(define (left-rotate r)
  (cdr r))

; Oef 9.4.2
(define (right-rotate r)
  (define (iter lst)
    (if (eq? (cdr lst) r)
        lst
        (iter (cdr lst))))
  (iter r))

; Oef 9.5

(define (cycles? r)
  (cond
    ((null? r) #f)
    ((eq? (memq (car r) (cdr r)) #f) #f)
    ((eq? r (memq (car r) (cdr r))) #t)
    (else #f)))