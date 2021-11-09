(define (square x)
  (* x x))

(define (fourth-* x)
  (* x x x x))

(define (fourth-square x)
  (* (square x) (square x)))

(define (sum-3-squares x y z)
  (+ (square x) (square y) (square z)))

(define (convert-c-to-f x)
  (- (* (+ x 40) 1.8) 40))

(define (convert-f-to-c x)
  (- (/ (+ x 40) 1.8) 40))

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

(define (scientific coefficient exponent)
  (* coefficient (expt 10 exponent)))

(define (sci-exponent getal)
  (floor (/ (log getal) (log 10))))

(define (sci-coefficient getal)
  (/ getal (expt 10 (sci-exponent getal))))

(define (discount prijs korting)
  (- prijs (* prijs (/ korting 100.0))))

(define (tip bedrag)
  (ceiling (* bedrag (/ 15 100.00))))