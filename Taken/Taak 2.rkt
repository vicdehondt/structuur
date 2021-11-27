; Structuur van Computerprogramma's - Taak 2

(#%require racket/trace)

(define (atom? x)
  (not (pair? x)))

; Oef 1 - Geneste lijsten: Minimum-Maximum
(define (minmax tree)
  (define (iter tree result)
    (cond
      ((null? tree) result)
      ((atom? tree) (cons (min (car result) tree) (max (cdr result) tree)))
      (else (cons (min (car (iter (car tree) result))
                       (car (iter (cdr tree) result)))
                  (max (cdr (iter (car tree) result))
                       (cdr (iter (cdr tree) result)))))))
  (if (null? tree)
      #f
      (iter tree (cons +inf.0 -inf.0))))

; Oef 2 - Familiebomen: Expressies Uitrekenen
(define (atom? x)
  (not (pair? x)))

(define (reken-uit expressie x)
  (define (change-x expression value)
    (cond
      ((null? expression) expression)
      ((and (atom? expression) (eq? expression 'x)) value)
      ((atom? expression) expression)
      (else (cons (change-x (car expression) value)
                  (change-x (cdr expression) value)))))

  (define (iter op tree ne)
    (cond
      ((null? tree) ne)
      ((atom? tree) tree)
      ((eq? (car tree) '+) (iter + (cdr tree) 0))
      ((eq? (car tree) '/) (iter / (cdr tree) 1))
      ((eq? (car tree) '*) (iter * (cdr tree) 1))
      (else (op (iter op (car tree) ne) (iter op (cdr tree) ne)))))
  (iter 'nil (change-x expressie x) 'nil))

; Oef 3 - Familiebomen: Expressies Uitrekenen - Delen door 0
(define (atom? x)
  (not (pair? x)))

(define (reken-uit expressie x)
  (define (change-x expression value)
    (cond
      ((null? expression) expression)
      ((and (atom? expression) (eq? expression 'x)) value)
      ((atom? expression) expression)
      (else (cons (change-x (car expression) value)
                  (change-x (cdr expression) value)))))

  (define (iter op tree ne)
    (cond
      ((null? tree) ne)
      ((atom? tree) tree)
      ((eq? (car tree) '+) (iter + (cdr tree) 0))
      ((eq? (car tree) '/) (iter / (cdr tree) 1))
      ((eq? (car tree) '*) (iter * (cdr tree) 1))
      (else (let ((left (iter op (car tree) ne))
                  (right (iter op (cdr tree) ne)))
              (cond
                ((or (not left) (not right)) #f)
                ((and (eq? op /) (eq? right 0)) #f)
                (else (op left right)))))))
  
  (iter 'nil (change-x expressie x) 'nil))





