
; Test
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