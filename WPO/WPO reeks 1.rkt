; 1.3.1
(define (square x)
  (* x x))

(define (fourth-* x)
  (* x x x x))

(define (fourth-square x)
  (* (square x) (square x)))

; 1.3.2
(define (sum-3-squares x y z)
  (+ (square x) (square y) (square z)))

; 1.3.3
(define (convert-c-to-f x)
  (- (* (+ x 40) 1.8) 40))

; 1.3.4
(define (convert-f-to-c x)
  (- (/ (+ x 40) 1.8) 40))

; 1.3.5
(define (oppervlakte-driehoek basis hoogte)
  (/ (* basis hoogte) 2))

(define (omtrek-driehoek zijde1 zijde2 zijde3)
  (+ zijde1 zijde2 zijde3))

(define (oppervlakte-vierkant zijde)
  (square zijde))

(define (omtrek-vierkant zijde)
  (* zijde 4))

(define (oppervlakte-cirkel r)
  (* (square r) 3.14))

(define (omtrek-cirkel r)
  (* 2 3.14 r))

; 1.3.6
(define (inhoud-balk lengte breedte hoogte)
  (* lengte breedte hoogte))

(define (oppervlakte-balk lengte breedte hoogte)
  (+ (* 2 (* lengte hoogte)) (* 2 (* breedte hoogte)) (* 2 (* lengte breedte))))

(define (inhoud-bol r)
  (* (/ 4 3) 3.14 r r r))

(define (oppervlakte-bol r)
  (* 4 3.14 (square r)))

(define (inhoud-cilinder h r)
  (* (square r) 3.14 h))

(define (oppervlakte-cilinder h r)
  (* 2 3.14 r (+ r h)))

; 1.7.1
(define (scientific coefficient exponent)
  (* coefficient (expt 10 exponent)))

; 1.7.2
(define (sci-exponent getal)
  (floor (/ (log getal) (log 10))))

(define (sci-coefficient getal)
  (/ getal (expt 10 (sci-exponent getal))))

; 1.8.1
(define (discount prijs korting)
  (- prijs (* prijs (/ korting 100.0))))

; 1.8.2
(define (tip bedrag)
  (ceiling (* bedrag (/ 15 100.00))))