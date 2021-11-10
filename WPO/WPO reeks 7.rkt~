(#%require racket/trace)

(define (atom? x)
  (not (pair? x)))

(define (leaf-count tree)
  (cond
    ((null? tree) 0)
    ((atom? tree) 1)
    (else (+ (leaf-count (car tree)) (leaf-count (cdr tree))))))

(define (depth tree)
  (cond
    ((null? tree) 0)
    ((atom? tree) 0)
    (else (max (+ 1 (depth (car tree))) (depth (cdr tree))))))

(define (depth-and-leaf-count tree)
  (cond
    ((null? tree) (cons 0 0))
    ((atom? tree) (cons 0 1))
    (else (let ((left (depth-and-leaf-count (car tree)))
                (right (depth-and-leaf-count (cdr tree))))
            (cons
             (max (+ 1 (car left))
                  (car right))
             (+ (cdr left) (cdr right)))))))

(define (fringe l)
  (cond
    ((null? l) '())
    ((atom? l) (list l))
    (else (append (fringe (car l)) (fringe (cdr l))))))

;fout
;(define (same-structure? l1 l2)
;  (cond
;    ((and (null? l1) (not (null? l2))) #f)
;    ((and (atom? l1) (atom? l2)) #t)
;    ((and (pair? (car l1)) (pair? (car l2))) (same-structure? (cdr l1) (cdr l2)))
;    ((not (= (length l1) (length l2))) #f)
;    ((and (pair? (car l1)) (atom? (car l2))) #f)
;    ((and (pair? (car l2)) (atom? (car l1))) #f)
;    (else #t)))

(define (same-structure? l1 l2)
  (cond
    ((and (null? l1) (not (null? l2))) #f)
    ((not (= (length l1) (length l2))) #f)
    ((and (atom? l1) (atom? l2)) #t)
    (else (same-structure? (cdr l1) (cdr l2)))))

(define (deep-combine combiner null-value l)
  (cond
    ((null? l) null-value)
    ((atom? l) l)
    (else (combiner (deep-combine combiner null-value (car l))
                    (deep-combine combiner null-value (cdr l))))))

(define (deep-map f l)
  (cond
    ((null? l) '())
    ((atom? l) (f l))
    (else (cons (deep-map f (car l))
                (deep-map f (cdr l))))))

(define (deep-change e1 e2 l)
  (deep-map (lambda (x) (if (eq? x e1) e2 x)) l))

(define (deep-atom-member? e l)
  (deep-combine (lambda (x y) (or x y))
                #f
                (deep-map (lambda (x) (eq? x e)) l)))

(define (count-atoms l)
  (deep-combine + 0 (deep-map (lambda (x) 1) l)))

(define boom
  '((blad (appel . golden))
    (blad (appel . granny))
    (((appel . golden) blad) blad (appel . cox))))

; gekeken op Reinout zijn scherm
(define (leafs boom)
  (cond
    ((null? boom) 0)
    ((and (atom? boom) (equal? boom 'blad)) 1)
    ((and (atom? boom) (not (equal? boom 'blad))) 0)
    (else (+ (leafs (car boom)) (leafs (cdr boom)))))) ; geen goede methode, hulpprocedures nodig

(define (all-apples boom)
  (cond
    ((null? boom) '())
    ((and (pair? boom) (equal? (car boom) 'appel)) (list (cdr boom)))
    ((and (atom? boom) (or (equal? boom 'blad) (equal? boom 'appel))) '())
    ((and (atom? boom) (not (and (equal? boom 'appel) (equal? boom 'blad)))) boom)
    (else (append (all-apples (car boom)) (all-apples (cdr boom))))))

(define (insert el set)
  (if (member el set)
      set
      (cons el set)))

(define (union set1 set2)
  (if (null? set1)
      set2
      (insert (car set1)
              (union (cdr set1) set2))))

;(define (apple-types boom)
;  (define (x result boom)
;    (define (member? result value)
;    (cond
;      ((null? result) #f)
;      ((equal? (car result) value) #t)
;      (else (member? (cdr result) value))))
;  (cond
;    ((null? boom) result)
;    ((blad? boom) '())
;    ((and (appel? boom) (member? result (cdr boom))) (append result (cdr boom)))
;    (else (append (x (car boom) result) (x (cdr boom) result)))))
;  (x '() boom))
;(trace apple-types)

;(define (apple-types boom)
;  (cond
;    ((null? boom) '())
;    ((and (pair? boom) (equal? (car boom) 'appel)) (list (cdr boom)))
;    ((and (atom? boom) (or (equal? boom 'blad) (equal? boom 'appel))) '())
;    ;((and (pair? boom) (equal? (car boom) 'blad)) '())
;    ((and (atom? boom) (not (and (equal? boom 'appel) (equal? boom 'blad)))) boom)
;    (else (union (apple-types (car boom)) (all-apples (cdr boom))))))

(define (blad? boom)
  (equal? boom 'blad))

(define (appel? boom)
  (equal? (car boom) 'appel))

(define (bewerk-boom boom doe-blad doe-appel combiner init)
  (cond
    ((null? boom) init)
    ((blad? boom) (doe-blad boom))
    ((appel? boom) (doe-appel boom))
    (else
     (combiner (bewerk-boom (car boom) doe-blad doe-appel combiner init)
               (bewerk-boom (cdr boom) doe-blad doe-appel combiner init)))))

(define (leafs-dmv-bewerk boom)
  (bewerk-boom boom (lambda (blad) 1) (lambda (appel) 0) + 0))

(define (all-apples-dmv-bewerk boom)
  (bewerk-boom boom (lambda (blad) '()) (lambda (appel) (list (cdr appel))) append '()))

(define (apple-types-dmv-bewerk boom)
  (bewerk-boom boom (lambda (blad) '()) (lambda (appel) (list (cdr appel))) union '()))

(define organigram
  '(directeur
    (hoofd-verkoop (verkoopsleider-vlaanderen)
                   (verkoopsleider-brussel))
    (hoofd-productie (hoofd-inkoop (bediende1)
                                   (bediende2)
                                   (bediende3))
                     (hoofd-fakturen))
    (hoofd-administratie (hoofd-personeel)
                         (hoofd-boekhouding))))

(define (baas organigram)
  (car organigram))

(define (sub-organigrammen organigram)
  (cdr organigram))

(define (bazen-van organigram p)
  (define (bazen-van organigram pad)
    (cond
      ((eq? (baas organigram) p) pad)
      (else (bazen-van-in (sub-organigrammen organigram) (cons (baas organigram) pad)))))
  (define (bazen-van-in lst pad)
    (cond
      ((null? lst) #f)
      (else (or (bazen-van (car lst) pad)
                (bazen-van-in (cdr lst) pad)))))
  (if (null? organigram)
      '()
      (bazen-van organigram '())))

;(trace bazen-van)

(define (hierarchisch? p1 p2 organigram)
  (define (hierarchisch? organigram pad)
    (cond
      ((and (eq? (baas organigram) p1) (member p2 pad)) #t)
      ((and (eq? (baas organigram) p2) (member p1 pad)) #t)
      (else (hierarchisch?-in (sub-organigrammen organigram) (cons (baas organigram) pad)))))
  (define (hierarchisch?-in lst pad)
    (cond
      ((null? lst) #f)
      (else (or (hierarchisch? (car lst) pad)
                (hierarchisch?-in (cdr lst) pad)))))
  (hierarchisch? organigram '()))

(define (collegas p organigram)
  (define (collegas organigram pad)
    (cond
      ((eq? (baas organigram) p) (append pad (werknemers-in (sub-organigrammen organigram))))
      (else (collegas-in (sub-organigrammen organigram) (cons (baas organigram) pad)))))
  (define (collegas-in lst pad)
    (if (null? lst)
        #f
        (or (collegas (car lst) pad) (collegas-in (cdr lst) pad))))
  (define (werknemers-in lst)
    (if (null? lst)
        '()
        (append (werknemers (car lst))
                (werknemers-in (cdr lst)))))
  (define (werknemers organigram)
    (cons (baas organigram)
          (werknemers-in (sub-organigrammen organigram))))
  (collegas organigram '()))

(define VUBOrganigram
  '(VUB (academisch (rectoraat)
                    (faculteiten
                     (rechten (bachelor (ba-rechten)
                                        (ba-criminologie))
                              (master (ma-rechten)
                                      (ma-criminologie)))
                     (economie)
                     (wetenschappen (bachelor (ba-wiskunde)
                                              (ba-fysica)
                                              (ba-cw))
                                    (master (ma-wiskunde)
                                            (ma-fysica)
                                            (ma-cw)))))
        (administratief (personeel) (financien))))

(define (display-n n d)
  (cond ((> n 0) (display d)
                 (display-n (- n 1) d))))
 
(define (print-lijn aantalblanco tekst)
  (display-n aantalblanco " ")
  (display tekst)
  (newline))

(define (hoofd lst)
  (car lst))

(define (print lst)
  (define (iter count lst)
    (cond
      ((atom? (car lst)) (print-lijn count (car lst)) (iter count (cdr lst)))
      ((pair? (car lst)) (print-lijn count (car (car lst)))
                               (iter (+ count 1) (cadr))
                               (iter (count) (cdr lst)))))
  (iter 0))

;(define (print-vanaf organigram label)
;  (define (print-vanaf organigram pad)
;    (cond
;      ((eq? (car organigram) label) (print organigram))
;      (else (display "test")))))
