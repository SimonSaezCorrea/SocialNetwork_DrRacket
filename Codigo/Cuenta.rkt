#lang racket

(require "date.rkt")
(require "Publicacion.rkt")

(provide getNombre_C)
(provide getContrasena_C)
(provide getFecha_C)
(provide getActividad_C)
(provide getFollow_C)
(provide getListFollow_C)
(provide getID_C)
(provide getListPublicaciones_C)

(provide account)

(provide account?)
(provide Publicaciones?)

(provide setActividad)
(provide addListFollow)
(provide addPublicacion)
(provide contadorPublicaciones)
(provide addPublicacion_encaps)
#|
Cuenta -> nombre | contrasena | fecha creacion | actividad | follows | list_follows | ID | listPublicaciones
          String | String | Date | Boolean | Integer | list account | Integer | list post
|#

;Constructor
(define (account nombre contrasena fecha actividad follow listFollow ID listPublicaciones)
  (if (and (string? nombre)
           (string? contrasena)
           (day? fecha)
           (boolean? actividad)
           (integer? follow)
           (integer? ID))
      (list nombre contrasena fecha actividad follow listFollow ID listPublicaciones)
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

(define (getListFollow_C cuenta)
  (car (cdr (cdr (cdr (cdr (cdr cuenta)))))))

(define (getID_C cuenta)
  (car (cdr (cdr (cdr (cdr (cdr (cdr cuenta))))))))

(define (getListPublicaciones_C cuenta)
  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr cuenta)))))))))


;Pertenencia

(define (account? cuenta)
  (if (string? (getNombre_C cuenta))
      (if (string? (getContrasena_C cuenta))
          (if (day? (getFecha_C cuenta))
              (if (boolean? (getActividad_C cuenta))
                  (if (integer? (getFollow_C cuenta))
                      (if (integer? (getID_C cuenta))
                          (if (Publicaciones? (getListPublicaciones_C cuenta))
                              #t
                              #f)
                          #f)
                      #f)
                  #f)
              #f)
          #f)
      #f))

(define (Publicaciones? Publicaciones)
  (if (not(null? Publicaciones))
      (if (post? (car Publicaciones))
          (Publicaciones? (cdr Publicaciones))
          #f)
      #t))
;Modificadores

(define (setActividad cuenta actividad)
  (if (account? cuenta)
      (if (boolean? actividad)
          (account (getNombre_C cuenta)
                   (getContrasena_C cuenta)
                   (getFecha_C cuenta)
                   actividad
                   (getFollow_C cuenta)
                   (getListFollow_C cuenta)
                   (getID_C cuenta)
                   (getListPublicaciones_C cuenta))
          cuenta)
      cuenta))



(define (addListFollow cuenta añadirCuenta)
  (if (and (account? cuenta)
           (account? añadirCuenta))
      (account (getNombre_C cuenta)
               (getContrasena_C cuenta)
               (getFecha_C cuenta)
               (getActividad_C cuenta)
               (+(getFollow_C cuenta) 1)
               (addListFollow_encaps (getListFollow_C cuenta) añadirCuenta)
               (getID_C cuenta)
               (getListPublicaciones_C))
      cuenta))

(define (addListFollow_encaps listCuenta añadirCuenta)
  (if (not(null? listCuenta))
      (cons (car listCuenta) (addListFollow_encaps (cdr listCuenta) añadirCuenta))
      (cons (car listCuenta) null)))




(define (addPublicacion cuenta publicacion)
  (if(and(account? cuenta)
         (post? publicacion))
     (account (getNombre_C cuenta)
              (getContrasena_C cuenta)
              (getFecha_C cuenta)
              (getActividad_C cuenta)
              (getFollow_C cuenta)
              (getListFollow_C cuenta)
              (getID_C cuenta)
              (addPublicacion_encaps (getListPublicaciones_C cuenta) publicacion))
     cuenta))
(define (addPublicacion_encaps Publicaciones publicacion)
  (if (not(null? Publicaciones))
      (cons (car Publicaciones) (addPublicacion_encaps (cdr Publicaciones) publicacion))
      (cons publicacion null)))




(define (contadorPublicaciones Publicaciones contador)
  (if (and (Publicaciones? Publicaciones)
           (integer? contador))
      (if (not (null? Publicaciones))
          (contadorPublicaciones (cdr Publicaciones) (+ contador 1))
          contador)
      null))