(#%require racket/trace)

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
(define (count-pairs lst)
  (let ((path '()))
    (define (count lst)
      (cond
        ((not (pair? lst)) 0)
        (else
         (set! path (cons lst path))
         (+ 1 (count (car lst)) (count (cdr lst))))))
    (count lst)))

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

; Oef 9.14

(define (ontdubbel! lst)
  (let ((even-lst '())
        (odd-lst '()))
    (define (hulp prev-even prev-odd lst)
      ;; lst = '() v even? (car lst) v odd? (car lst)
      (cond
        ((null? lst) (set-cdr! prev-even '())
                     (set-cdr! prev-odd '())
                     (cons even-lst odd-lst))
        ((even? (car lst)) (if (null? prev-even)
                               (set! even-lst lst)
                               (set-cdr! prev-even lst))
                           (hulp lst prev-odd (cdr lst)))
        ((odd? (car lst)) (if (null? prev-odd)
                              (set! odd-lst lst)
                              (set-cdr! prev-odd lst))
                          (hulp prev-even lst (cdr lst)))))
    (hulp even-lst odd-lst lst)))

; Oef 9.15
(define (insert! lst1 lst2)
  (define (last-element-of lst)
    (if (null? (cdr lst))
        lst
        (last-element-of (cdr lst))))
  (let loop
    ((current lst1)
     (adden lst2))
    (if (null? current)
        lst1
        (begin (set-cdr! (last-element-of (car current)) (list (car adden)))
               (loop (cdr current) (cdr adden))))))
(define (add! lst1 lst2)
  (set-cdr! lst2 '())
  (if (null? (cdr lst1))
      (set-cdr! lst1 lst2)
      (add! (cdr lst1) lst2)))

(define (insert! lst1 lst2)
  (if (not (null? lst1))
      (begin
        (insert! (cdr lst1) (cdr lst2))
        (add! (car lst1) lst2))))

; Oef 9.17
(define best1 '((ann (meiboomstraat 12 1820 Eppegem))
                (bert (populierendreef 7 1050 Brussel))
                (kurt (Mechelsesteenweg 50 1800 Vilvoorde))))
 
(define best2 '((bert (populierendreef 7 1050 Brussel))
                (ernie (eikestraat 1 9000 Gent))
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











