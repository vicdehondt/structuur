
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
    ((eq? r (memq (car r) (cdr r))) #t)
    (else (memq (car r) (cdr r)))))

; Oef 9.8


; Oef 9.13
(define lijst1 '(1 3 5))
(define lijst2 '(2 4 6 8))

(define (schuif-in! lst1 lst2)
  (cond
    ((null? (cdr lst1)) (set-cdr! lst1 lst2)
                        (display "ok"))
    ((not (null? lst2)) (let ((rest-lst1 (cdr lst1))
                              (rest-lst2 (cdr lst2)))
                          (set-cdr! lst1 lst2)
                          (set-cdr! lst2 rest-lst1)
                          (schuif-in! rest-lst1 rest-lst2)))))

; Oef 9.17
(define best1 '((ann (meiboomstraat 12 1820 Eppegem))
                (bert (populierendreef 7 1050 Brussel))
                (kurt (Mechelsesteenweg 50 1800 Vilvoorde))))
 
(define best2 '((bert (populierendreef 7 1050 Brussel))
                (jan (eikestraat 1 9000 Gent))
                (sofie (boerendreef 5  2800 Mechelen))))

; Oef 9.17.2
(define (symbol<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))
 
(define (element=? el1 el2)
  (equal? el1 el2))

(define (merge best1 best2)
  (define (merge-in current rest1 rest2)
  (cond
    ((null? rest1) (set-cdr! current rest2))
    ((null? rest2) (set-cdr! current rest1))
    ((element=? (caar rest1) (caar rest2)) (set-cdr! current rest1)
                                           (merge-in rest1 (cdr rest1) (cdr rest2)))
    ((symbol<? (caar rest1) (caar rest2)) (set-cdr! current rest1)
                                          (merge-in rest1 (cdr rest1) rest2))
    (else (set-cdr! current rest2)
          (merge-in rest2 rest1 (cdr rest2)))))
  (let* ((current (if (symbol<? (caar best1) (caar best2))
                     best1
                     best2))
        (rest1 (cdr current))
        (rest2 (if (eq? current best1) best2 best1)))
    (merge-in current rest1 rest2)
    current))








