#lang racket
#|
Cuenta -> nombre | contrasena | fecha creacion | actividad | follows
          String | String | Date | Boolean | Integer
|#

;Constructor
(define (cuenta nombre contrasena fecha actividad follow)
  (list nombre contrasena fecha actividad follow))

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


;Pertenencia

(define (cuenta? cuenta)
  (if (string? (getNombre_C cuenta))
      (if (string? (getContrasena_C cuenta))
          (if (date? (getFecha_C cuenta))
              (if (boolean? (getActividad_C cuenta))
                  (if (integer? (getFollow_C cuenta))
                      #t
                      #f)
                  #f)
              #f)
          #f)
      #f))


;Modificadores

(define (setActividad cuenta actividad)
  (if (cuenta? cuenta)
      (if (boolean? actividad)
          (list (getNombre_C cuenta) (getContrasena_C cuenta) (getFecha_C cuenta) actividad (getFollow_C cuenta))
          null)
      null))

(define (setFollow cuenta follow)
  (if (cuenta? cuenta)
      (if (integer? follow)
          (list (getNombre_C cuenta) (getContrasena_C cuenta) (getFecha_C cuenta) (getActividad_C cuenta) follow)
          null)
      null))