(#%require racket/trace)

; 7.1
(define (atom? x)
  (not (pair? x)))

; 7.2.1
(define (leaf-count tree)
  (cond
    ((null? tree) 0)
    ((atom? tree) 1)
    (else (+ (leaf-count (car tree)) (leaf-count (cdr tree))))))

; 7.2.2
(define (depth tree)
  (cond
    ((null? tree) 0)
    ((atom? tree) 0)
    (else (max (+ 1 (depth (car tree))) (depth (cdr tree))))))

; 7.2.3
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

; 7.3
(define (fringe l)
  (cond
    ((null? l) '())
    ((atom? l) (list l))
    (else (append (fringe (car l)) (fringe (cdr l))))))

; 7.5
(define (same-structure? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((or  (null? l1) (null? l2)) #f)
        ((and (atom? l1) (atom? l2)) #t)
        ((or  (atom? l1) (atom? l2)) #f)
        (else (and (same-structure? (car l1) (car l2))
                   (same-structure? (cdr l1) (cdr l2))))))
; of:
(define (same-structure?-or l1 l2)
  (or (and (null? l1) (null? l2))
      (and (atom? l1) (atom? l2))
      (and (pair? l1)
           (pair? l2)
           (same-structure?-or (car l1) (car l2))
           (same-structure?-or (cdr l1) (cdr l2)))))

; 7.6.1
(define (deep-combine combiner null-value l)
  (cond
    ((null? l) null-value)
    ((atom? l) l)
    (else (combiner (deep-combine combiner null-value (car l))
                    (deep-combine combiner null-value (cdr l))))))

; 7.6.2
(define (deep-map f l)
  (cond
    ((null? l) '())
    ((atom? l) (f l))
    (else (cons (deep-map f (car l))
                (deep-map f (cdr l))))))

; 7.6.3
(define (deep-change e1 e2 l)
  (deep-map (lambda (x) (if (eq? x e1) e2 x)) l))

; 7.6.4
(define (deep-atom-member? e l)
  (deep-combine (lambda (x y) (or x y))
                #f
                (deep-map (lambda (x) (eq? x e)) l)))

; 7.6.5
(define (count-atoms l)
  (deep-combine + 0 (deep-map (lambda (x) 1) l)))

; 7.9
(define boom
  '((blad (appel . golden))
    (blad (appel . granny))
    (((appel . golden) blad) blad (appel . cox))))

; 7.9.1
; gekeken op Reinout zijn scherm
(define (leafs boom)
  (cond
    ((null? boom) 0)
    ((and (atom? boom) (equal? boom 'blad)) 1)
    ((and (atom? boom) (not (equal? boom 'blad))) 0)
    (else (+ (leafs (car boom)) (leafs (cdr boom)))))) ; geen goede methode, hulpprocedures nodig

; 7.9.2
(define (all-apples boom)
  (cond
    ((null? boom) '())
    ((and (pair? boom) (equal? (car boom) 'appel)) (list (cdr boom)))
    ((and (atom? boom) (or (equal? boom 'blad) (equal? boom 'appel))) '())
    ((and (atom? boom) (not (and (equal? boom 'appel) (equal? boom 'blad)))) boom)
    (else (append (all-apples (car boom)) (all-apples (cdr boom))))))

; 7.9.3
(define (insert el set)
  (if (member el set)
      set
      (cons el set)))

(define (union set1 set2)
  (if (null? set1)
      set2
      (insert (car set1)
              (union (cdr set1) set2))))

(define (apple-types boom)
  (cond ((null? boom) '())
        ((blad? boom) '())
        ((appel? boom) (list (cdr boom)))
        (else (union (apple-types (car boom))
                     (apple-types (cdr boom))))))

(define (blad? boom)
  (equal? boom 'blad))

(define (appel? boom)
  (equal? (car boom) 'appel))

; 7.9.4
(define (bewerk-boom boom doe-blad doe-appel combiner init)
  (cond
    ((null? boom) init)
    ((blad? boom) (doe-blad boom))
    ((appel? boom) (doe-appel boom))
    (else
     (combiner (bewerk-boom (car boom) doe-blad doe-appel combiner init)
               (bewerk-boom (cdr boom) doe-blad doe-appel combiner init)))))

; 7.9.5
(define (leafs-dmv-bewerk boom)
  (bewerk-boom boom (lambda (blad) 1) (lambda (appel) 0) + 0))

; 7.9.6
(define (all-apples-dmv-bewerk boom)
  (bewerk-boom boom (lambda (blad) '()) (lambda (appel) (list (cdr appel))) append '()))

; 7.9.7
(define (apple-types-dmv-bewerk boom)
  (bewerk-boom boom (lambda (blad) '()) (lambda (appel) (list (cdr appel))) union '()))

; 7.11
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

; 7.11.1
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

; 7.11.2
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

; 7.11.3
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

; 7.13
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

; 7.13.1
(define takken cdr)
(define lbl car)

(define (print-vanaf organigram label)
  (define (find-label organigram)
    (if (eq? (lbl organigram) label)
        organigram
        (find-label-in (takken organigram))))

  (define (find-label-in lst)
    (if (null? lst)
        #f
        (or (find-label (car lst))
            (find-label-in (cdr lst)))))

  (define (print organigram diepte)
    (print-lijn diepte (lbl organigram))
    (print-in (takken organigram) (+ diepte 1)))

  (define (print-in lst diepte)
    (if (not (null? lst))
        (begin
          (print (car lst) diepte)
          (print-in (cdr lst) diepte))))

  (let ((to-print (find-label organigram)))
    (if to-print
        (print to-print 0)
        #f)))

; 7.13.2
(define (print-tot organigram niveau)
  (define (print-tot organigram diepte)
    (cond
      ((<= diepte niveau)
       (print-lijn diepte (car organigram))
       (print-tot-in (cdr organigram) (+ diepte 1)))))

  (define (print-tot-in lst diepte)
    (cond
      ((not (null? lst))
       (print-tot (car lst) diepte)
       (print-tot-in (cdr lst) diepte))))

  (print-tot organigram 0))

; 7.17
(define familieboom '(jan (piet (frans (tom)
                                       (roel))
                                (mie))
                          (bram (inge (bert (ina)
                                            (ilse))
                                      (bart))
                                (iris))
                          (joost (else (ilse)))))

; 7.17.1
(define (verdeel-democratisch familieboom budget)
  (define (tel familieboom)
    (if (not (pair? (cdr familieboom)))
        1
        (+ 1 (tel-in (cdr familieboom))))) ; kan een stuk korter: delete "" en werkt even goed

  (define (tel-in lst)
    (if (not (null? lst))
        (+ (tel (car lst))
           (tel-in (cdr lst)))
        0))

  (/ budget (- (tel familieboom) 1))) ;of (tel-in (cdr familiebomen), dan skip je de bovenste
                                      ;persoon, die je er anders toch aftrekt

; 7.17.2
(define (budget familieboom budgettenlijst)
  (define (budget familieboom budgettenlijst)
       (if (null? budgettenlijst)
           0
           (+ (car budgettenlijst) (budget-in (cdr familieboom) (cdr budgettenlijst)))))

  (define (budget-in lst budgettenlijst)
    (if (null? lst)
        0
        (+ (budget (car lst) budgettenlijst)
           (budget-in (cdr lst) budgettenlijst))))  

  (budget-in (cdr familieboom) budgettenlijst))
       
; 7.17.3
(define (verdeel familieboom budget)
  (define (zonder-kind? familieboom)
    (null? (cdr familieboom)))
  
  (define (verdeel familieboom budget)
    (if (zonder-kind? familieboom)
        (list (list (car familieboom) budget))
        (verdeel-in (cdr familieboom) (/ budget (length (cdr familieboom))))))

  (define (verdeel-in lst budget)
    (if (null? lst)
        '()
        (append (verdeel (car lst) budget)
                (verdeel-in (cdr lst) budget))))

  (verdeel-in (cdr familieboom) (/ budget (length (cdr familieboom)))))
