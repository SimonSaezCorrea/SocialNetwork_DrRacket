#lang racket

(require "date.rkt")
(require "Cuenta.rkt")
(require "Publicacion.rkt")


;Constructor

(define (socialnetwork name date encryptFunction decryptFunction)
  (if (and (string? name)
           (day? date))
      (list name date encryptFunction decryptFunction (list) (list))
      null))

;Selectores

(define (getName_SN SN)
  (car SN))

(define (getDate_SN SN)
  (car (cdr SN)))

(define (getEncryptFunction_SN SN)
  (car (cdr (cdr SN))))

(define (getDecryptFunction_SN SN)
  (car (cdr (cdr (cdr SN)))))

(define (getCuenta_SN SN)
  (car (cdr (cdr (cdr (cdr SN))))))

(define (getPublicacion_SN SN)
  (car (cdr (cdr (cdr (cdr (cdr SN)))))))

;Pertenencia

(define (Cuentas? Cuentas_SN)
  (if (nor(null? (car(Cuentas_SN))))
      (if (account? (car(Cuentas_SN)))
          (Cuentas? (cdr Cuentas_SN))
          #f)
      #t))
(define (Publicaciones? Publicaciones_SN)
  (if (nor(null? (car(CuentasPublicaciones_SN_SN))))
      (if (post? (car(Publicaciones_SN)))
          (Publicaciones? (cdr Publicaciones_SN))
          #f)
      #t))
      

(define (socialnetwork? SN)
  (if (and (string? (getName_SN SN))
           (day? (getDate_SN SN))
           (Cuentas? (getCuenta_SN SN))
           (Publicaciones? (getPublicaciones_SN SN)))
      #t
      #f))

;Modificador

(define (addCuenta_SN SN cuenta)
  (if(and(socialnetwork? SN)
         (account? cuenta))
     (addCuenta (getCuenta_SN SN) cuenta)
     null))
(define (addCuenta Cuentas cuenta)
  (if (not(null? Cuentas))
      (cons (car Cuentas) (addCuenta (cdr Cuentas) cuenta))
      (cons cuenta null)))


(define (addPublicacion_SN SN publicacion)
  (if(and(socialnetwork? SN)
         (post? publicacion))
     (addPublicacion (getPublicacion_SN SN) publicacion)
     null))
(define (addPublicacion Publicaciones publicacion)
  (if (not(null? CuentPublicacionesas))
      (cons (car Publicaciones) (addPublicacion (cdr Publicaciones) publicacion))
      (cons publicacion null)))

;Otras funciones

(define (contadorCuentas Cuentas contador)
  (if (and (Cuentas? Cuentas)
           (integer? contador))
      (if (not (null? Cuentas))
          (contadorCuentas (cdr Cuentas) (+ contador 1))
          contador)
      null))

(define (contadorPublicaciones Publicaciones contador)
  (if (and (Publicaciones? Publicaciones)
           (integer? contador))
      (if (not (null? Publicaciones))
          (contadorPublicaciones (cdr Publicaciones) (+ contador 1))
          contador)
      null))