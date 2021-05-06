#lang racket

(require "date.rkt")

(provide account)
(provide account?)
(provide setActividad)
(provide setFollow)

#|
Cuenta -> nombre | contrasena | fecha creacion | actividad | follows | ID
          String | String | Date | Boolean | Integer | Integer
|#

;Constructor
(define (account nombre contrasena fecha actividad follow ID)
  (if (and (string? nombre)
           (string? contrasena)
           (day? fecha)
           (boolean? actividad)
           (integer? follow)
           (integer? ID))
      (list nombre contrasena fecha actividad follow ID)
      null))

;Selector

(define (getNombre_C cuenta)
  (car cuenta))

(define (getContrasena_C cuenta)
  (car (cdr cuenta)))

(define (getFecha_C cuenta)
  (car (cdr (cdr cuenta))))

(define (getActividad_C cuenta)
  (car (cdr (cdr (cdr cuenta)))))

(define (getFollow_C cuenta)
  (car (cdr (cdr (cdr (cdr cuenta))))))

(define (getID_C cuenta)
  (car (cdr (cdr (cdr (cdr (cdr cuenta)))))))


;Pertenencia

(define (account? cuenta)
  (if (string? (getNombre_C cuenta))
      (if (string? (getContrasena_C cuenta))
          (if (day? (getFecha_C cuenta))
              (if (boolean? (getActividad_C cuenta))
                  (if (integer? (getFollow_C cuenta))
                      (if (integer? (getID_C cuenta))
                          #t
                          #f)
                      #f)
                  #f)
              #f)
          #f)
      #f))


;Modificadores

(define (setActividad cuenta actividad)
  (if (account? cuenta)
      (if (boolean? actividad)
          (account (getNombre_C cuenta) (getContrasena_C cuenta) (getFecha_C cuenta) actividad (getFollow_C cuenta) (getID_C cuenta))
          cuenta)
      cuenta))

(define (setFollow cuenta follow)
  (if (account? cuenta)
      (if (integer? follow)
          (account (getNombre_C cuenta) (getContrasena_C cuenta) (getFecha_C cuenta) (getActividad_C cuenta) follow (getID_C cuenta))
          cuenta)
      cuenta))