#lang racket

(provide (all-defined-out))

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

;Otras funciones

#|
Des: Permite crear un string con el contenido de una Fecha
Dom: Fecha y un string
Rec: String
|#
(define (string->fecha fecha)
  (string-append "La fecha de creacion es " (number->string (getDia fecha)) "/" (number->string (getMes fecha)) "/" (number->string (getAno fecha)) "\n"))