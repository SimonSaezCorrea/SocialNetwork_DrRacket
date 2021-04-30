#lang racket

(provide day)
(provide day?)


#|
Date -> dia | mes | ano
        Integer | Integer | Integer
|#

;Construcor

(define (day dia mes ano)
  (list dia mes ano))


;Selectores

(define (getDia fecha)
  (car fecha))

(define (getMes fecha)
  (car (cdr fecha)))

(define (getAno fecha)
  (car (cdr (cdr fecha))))


;Pertenencia

(define (day? fecha)
  (if (and (integer? (getDia fecha))
           (< 0 (getDia fecha))
           (> 32 (getDia fecha))
           (integer? (getMes fecha))
           (< 0 (getMes fecha))
           (> 13 (getMes fecha))
           (integer? (getAno fecha))
           (< 0 (getAno fecha)))
      #t
      #f))