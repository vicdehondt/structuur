(#%require racket/trace)

(define (accumulate lst op ne)
  (if (null? lst)
      ne
      (op (car lst)
          (accumulate (cdr lst) op ne))))

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst)
               (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define flip
  (let ((counter 0))
    (if (zero? counter)
        (lambda ()
          (set! counter (if (zero? counter) 1 0))
          counter))))

(define (make-flip)
  (let ((counter 0))
    (if (zero? counter)
        (lambda ()
          (set! counter (if (zero? counter) 1 0))
          counter))))

;(define (make-random m a seed)
;  (let ((x_i seed)
;        (x_i_1 0))
;    (lambda ()
;      (set! x_i (modulo (* x_i a) m))
;      (set! x_i_1 (/ x_i m))
;      (exact->inexact x_i_1))))

(define (make-random m a seed)
  (define (generate)
    (set! seed (modulo (* seed a) m))
    (exact->inexact (/ seed m)))
  (define (reset new)
    (set! seed new))
  (define (dispatch m)
    (cond
      ((eq? m 'generate) generate)
      ((eq? m 'reset) reset)
      (else (display "fault"))))
  dispatch)

(define (make-counter initial)
  (define (increase!)
    (set! initial (+ initial 1)))
  (define (decrease!)
    (set! initial (- initial 1)))
  (define (dispatch m)
    (cond ((eq? m 'increase!) increase!)
          ((eq? m 'decrease!) decrease!)
          ((eq? m 'read) initial)
          (else (display "wrong message"))))
  dispatch)

(define (make-parking capacity1 capacity2)
  (define level1 (make-counter 0))
  (define level2 (make-counter 0))
  
  (define (level1-full)
    (= (level1 'read) capacity1))

  (define (level2-full)
    (= (level2 'read) capacity2))

  (define (level1-empty)
    (= (level1 'read) 0))

  (define (level2-empty)
    (= (level2 'read) 0))

  (define (full?)
    (and (level1-full) (level2-full)))

  (define (empty?)
    (and (level1-empty) (level2-empty)))

  (define (level)
    (cond
      ((not (level1-full)) 1)
      ((not (level2-full)) 2)))

  (define (car-enters)
    (cond
      ((not (level1-full)) ((level1 'increase!)))
      ((not (level2-full)) ((level2 'increase!)))
      (else #f)))
  
  (define (car-leaves)
    (cond
      ((not (level2-empty)) ((level2 'decrease!)))
      ((not (level1-empty)) ((level1 'decrease!)))
      (else #f)))
  
  (define (dispatch m)
    (cond
      ((eq? m 'full?) (full?))
      ((eq? m 'empty?) (empty?))
      ((eq? m 'level) (level))
      ((eq? m 'car-enters!) car-enters)
      ((eq? m 'car-leaves!) car-leaves)
      (else (display "ERROR wrong message!"))))
  dispatch)

(define (maak-laadstation)

  (define gekoppeld? #f)

  (define elec-withdrawn 0)

  (define (withdraw! amount)
    (set! elec-withdrawn amount))

  (define (koppel!)
    (set! gekoppeld? #t))

  (define (ontkoppel!)
    (set! gekoppeld? #f))

  (define (vrij?)
    (equal? gekoppeld? #f))
  
  (define (dispatch m)
    (cond
      ((eq? m 'withdraw!) withdraw!)
      ((eq? m 'total-withdrawn) elec-withdrawn)
      ((eq? m 'koppel!) (koppel!))
      ((eq? m 'ontkoppel!) (ontkoppel!))
      ((eq? m 'vrij?) (vrij?))
      (else (display "ERROR wrong message!"))))
  dispatch)

; Niet juiste methode:
(define (maak-auto capaciteit)
  
  (define percentage 50)
  
  (define laadstation #f)
    
  (define (charge) percentage)

  (define (charge!)
    (if laadstation
        (let ((amount (* (/ (- 100 percentage) 100) capaciteit)))
          ((laadstation 'withdraw!) amount)
          (set! percentage 100))
        (display "Not connected to charger!")))
    
  (define (koppel! station)
    (if (not laadstation)
        (set! laadstation station)
        ((laadstation 'koppel!) dispatch)))
    
  (define (ontkoppel!)
    (set! laadstation #f))

  (define (dispatch m)
    (cond
      ((eq? m 'charge) (charge))
      ((eq? m 'charge!) (charge!))
      ((eq? m 'koppel!) koppel!)
      ((eq? m 'ontkoppel!) (ontkoppel!))
      (else "ERROR UNKNOWN MESSAGE")))
  dispatch)
; Liever vars in een let

; Sander:
(define (maak-auto capaciteit)
  (let ((percentage 50)
        (laadstation #f))
    (define (charge)
      percentage)
    (define (charge!)
      (if laadstation
          (let ((amount (* (/ (- 100 percentage) 100) capaciteit)))
            ((laadstation 'withdraw!) amount)
            (set! percentage 100))))
    (define (koppel! station)
      (if (not laadstation)
          (set! laadstation station)
          ((laadstation 'koppel!) dispatch)))
    (define (ontkoppel!)
      (set! laadstation #f))
    (define (dispatch m)
      (cond
        ((eq? m 'charge) (charge))
        ((eq? m 'charge!) (charge!))
        ((eq? m 'koppel!) koppel!)
        ((eq? m 'ontkoppel!) (ontkoppel!))
        (else "ERROR UNKNOWN MESSAGE")))
    dispatch))

(define (maak-laadpark n)

  (define (make-stations n)
    (if (zero? n)
        '()
        (cons (maak-laadstation)
              (make-stations (- n 1)))))

  (let ((stations (make-stations n)))
    
    (define (full?)
      (null? (filter (lambda (station) ((station 'vrij?))) stations)))

    (define (enter!)
      (let ((vrije-stations (filter (lambda (station) ((station 'vrij?))) stations)))
        (if (not (null? vrije-stations))
            ((car 'koppel!) (car vrije-stations))
            #f)))


    (define (dispatch m)
      (cond
        ((eq? m 'full?) full?)
        ((eq? m 'enter!) enter!)
        (else (display "ERROR wrong message!"))))
    dispatch))

; tests
(define (test num actual expected)
  (if (equal? actual expected)
      (begin
        (display "Test")
        (display num)
        (display " passed")
        (newline))
      (begin
        (display "Test")
        (display num)
        (display " failed ")
        (display actual)
        (newline))))

; parking tests
(display "PARKING")
(newline)

(define parking (make-parking 3 5))

(test 1 (parking 'level) 1)
(test 2 (parking 'full?) #f)

((parking 'car-enters!))
((parking 'car-enters!))
((parking 'car-enters!))
((parking 'car-enters!))

(test 3 (parking 'level) 2)
(test 4 (parking 'empty?) #f)

((parking 'car-enters!))
((parking 'car-enters!))
((parking 'car-enters!))
((parking 'car-enters!))

(test 5 (parking 'full?) #t)
(test 6 ((parking 'car-enters!)) #f)

((parking 'car-leaves!))
((parking 'car-leaves!))
((parking 'car-leaves!))
((parking 'car-leaves!))
((parking 'car-leaves!))
((parking 'car-leaves!))

(test 7 (parking 'level) 1)

(newline)

; laadstation tests
(display "LAADSTATION")
(newline)

(define batterij (maak-laadstation))

(test 1 (batterij 'vrij?) #t)

((batterij 'withdraw!) 50)
(test 2 (batterij 'total-withdrawn) 50)

(batterij 'koppel!)
(test 3 (batterij 'vrij?) #f)

(batterij 'ontkoppel!)
(test 4 (batterij 'vrij?) #t)

(newline)

; auto tests
(display "AUTO")
(newline)

(define tesla (maak-auto 100))

(test 1 (tesla 'charge) 50)

((tesla 'koppel!) batterij)

(tesla 'charge!)

(test 2 (tesla 'charge) 100)

(tesla 'ontkoppel!)

