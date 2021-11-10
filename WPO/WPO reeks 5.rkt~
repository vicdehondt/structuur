(#%require racket/trace)

(define (add-to-end e l)
  (if (null? l)
      (list e)
      (cons (car l) (add-to-end e (cdr l)))))

;(define (append x y)
;  (define (iter y res)
;    (if (null? y)
;        (reverse res)
;        (iter (cdr y) (cons (car y) res))))
;  (iter y (reverse x)))
; kan met 1 reverse minder, zoals:

(define (append x y)
  (define (iter x res)
    (if (null? x)
        res
        (iter (cdr x) (cons (car x) res))))
  (iter (reverse x) y))

(define (reverse l)
  (if (null? l)
      '()
      (append (reverse (cdr l)) (cons (car l) '()))))

(define (iter-reverse l)
  (define (iter l res)
    (if (null? l)
        res
        (iter (cdr l) (cons (car l) res))))
  (iter l '()))

(define (last list)
  (define (iter result)
    (cond
      ((null? result) #f)
      ((null? (cdr result)) (car result))
      (else (iter (cdr result)))))
  (iter list))

(define (change e1 e2 l)
  (cond
    ((eq? e1 e2) l)
    ((null? l) l)
    ((null? (car l)) l)
    ((eq? (car l) e1) (change e1 e2 (cons e2 (cdr l))))
    (else (cons (car l) (change e1 e2 (cdr l))))))

(define (change-dmv-map e1 e2 l)
  (map (lambda (x) (if (eq? e1 x)
                       e2
                       x)) l))

(define (my-equal? l1 l2)
  (define trues 1)
  (cond
    ((not (= (length l1) (length l2))) #f)
    ((and (null? l1) (null? l2)) #t)
    ((or (and (null? l1) (not (null? l2))) (and (null? l2) (not (null? l1)))) #f)
    (else (if (= trues (and (length l1) (length l2)))
          #t
          (if (eq? (car l1) (car l2))
              (begin
                (+ trues 1)
                (my-equal? (cdr l1) (cdr l2)))
              #f)))))

(define (rec-sum-lists l1 l2)
  (if (or (null? l1) (null? l2))
      (append l1 l2)
        (append (list (+ (car l1) (car l2))) (rec-sum-lists (cdr l1) (cdr l2)))))

(define (iter-sum-lists l1 l2)
  (define (iter list1 list2 result)
    (cond
      ((null? list1) (append result list2))
      ((null? list2) (append result list1))
      ((and (null? list1) (null? list2)) result)
      (else (iter (cdr list1) (cdr list2) (append result (list (+ (car list1) (car list2))))))))
  (iter l1 l2 '()))

;(define (rec-merge-n lst1 lst2 n)
;  (define (merge lst1 lst2 count)
;    (trace merge)
;    (if (= count n)
;        (if (and (<= (length lst1) n) (<= (length lst2) n))
;            (append lst2 lst1)
;            (merge lst2 lst1 0))
;        (list (car lst1) (merge (cdr lst1) lst2 (+ count 1)))))
;  (cond
;    ((and (<= (length lst1) n) (<= (length lst2) n)) (append lst1 lst2)) ;beide lijsten <= n
;    ((and (> (length lst1) n) (<= (length lst2) n)) (merge lst1 lst2 0))))

; Sander:
(define (rec-merge-n lst1 lst2 n)
  (define (merge lst1 lst2 i)
    (cond
      ((null? lst1) lst2)
      ((= i n) (merge lst2 lst1 0))
      (else (cons (car lst1)
                  (merge (cdr lst1) lst2 (+ i 1))))))
  (merge lst1 lst2 0))

(define (iter-merge-n lst1 lst2 n)
  (define (iter lst1 lst2 i result)
    (cond
      ((null? lst1) (append (reverse result) lst2))
      ((= i n) (iter lst2 lst1 0 result))
      (else (iter (cdr lst1) lst2 (+ i 1) (cons (car lst1) result)))))
  (iter lst1 lst2 0 '()))

;(define (super-merge-n lsts n)
;  ()
;  (define (merge lists count listcount)
;    (if ())
;    (cond
;      ((null? lists) '())
;      ((null? (car lists)) (merge (cdr lists) 0 (+ listcount 1)))
;      ((= count n) (merge (cdr lists) 0 (+ listcount 1)))
;      ((and (= count n) (null? (cdr lists))) (merge lsts))
;      (else ))))


; Sander:
(define (super-merge-n lsts n)
  (define (merge lst rest i)
    (cond
      ((and (null? lst) (null? rest)) '())
      ((null? lst) (merge (car rest) (cdr rest) 0))
      ((= i n) (merge (car rest) (append (cdr rest) (list lst)) 0))
      (else (cons (car lst)
                  (merge (cdr lst) rest (+ i 1))))))
  (if (null? lsts)
      '()
      (merge (car lsts) (cdr lsts) 0)))