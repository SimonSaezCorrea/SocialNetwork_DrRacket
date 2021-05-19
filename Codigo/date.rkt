#lang racket

(provide day)
(provide day?)


#|
Date -> dia | mes | ano
        Integer | Integer | Integer
|#

;Construcor

#|
Des: Es el constructor de un day
Dom: Dia (Integer), Mes (Integer), Ano (Integer)
Rec: Una lista con los datos guardados y comprobados
|#
(define (day dia mes ano)
  (if (and (integer? dia)
           (< 0 dia)
           (> 32 dia)
           (integer? mes)
           (< 0 mes)
           (> 13 mes)
           (integer? ano)
           (< 0 ano))
      (list dia mes ano)
      null))


;Selectores

#|
Des: Permite obtener el dia de la fecha
Dom: El dato Day (Una fecha)
Rec: El dato dia
|#
(define (getDia fecha)
  (car fecha))

#|
Des: Permite obtener el mes de la fecha
Dom: El dato Day (Una fecha)
Rec: El dato mes
|#
(define (getMes fecha)
  (car (cdr fecha)))

#|
Des: Permite obtener el ano de la fecha
Dom: El dato Day (Una fecha)
Rec: El dato ano
|#
(define (getAno fecha)
  (car (cdr (cdr fecha))))


;Pertenencia

#|
Des: Permite saber si corresponde a una fecha
Dom: El dato Day (Una fecha)
Rec: Sentencia booleana
|#
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