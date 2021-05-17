#lang racket

(require "date.rkt")
(require "Cuenta.rkt")
(require "Publicacion.rkt")


(provide socialnetwork)

(provide getCuenta_SN)
(provide getEncryptFunction_SN)
(provide getDecryptFunction_SN)
(provide getDate_SN)
(provide getName_SN)

(provide Cuentas?)
(provide socialnetwork?)

(provide addCuenta_SN)
(provide contadorCuentas)
(provide activar)
(provide buscarCuentaActiva)
(provide desactivar)

;Constructor

(define (socialnetwork name date encryptFunction decryptFunction)
  (if (and (string? name)
           (day? date))
      (list name date encryptFunction decryptFunction (list))
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

;Pertenencia

(define (Cuentas? Cuentas_SN)
  (if (not(null? Cuentas_SN))
      (if (account? (car Cuentas_SN))
          (Cuentas? (cdr Cuentas_SN))
          #f)
      #t))

(define (socialnetwork? SN)
  (if (and (string? (getName_SN SN))
           (day? (getDate_SN SN))
           (Cuentas? (getCuenta_SN SN)))
      #t
      #f))

;Modificador

(define (addCuenta_SN SN cuenta)
  (if(and(socialnetwork? SN)
         (account? cuenta))
     (list (getName_SN SN)
                    (getDate_SN SN)
                    (getEncryptFunction_SN SN)
                    (getDecryptFunction_SN SN)
                    (addCuenta (getCuenta_SN SN) cuenta))
     SN))
(define (addCuenta Cuentas cuenta)
  (if (not(null? Cuentas))
      (cons (car Cuentas) (addCuenta (cdr Cuentas) cuenta))
      (cons cuenta null)))

;Otras funciones

(define (contadorCuentas Cuentas contador)
  (if (and (Cuentas? Cuentas)
           (integer? contador))
      (if (not (null? Cuentas))
          (contadorCuentas (cdr Cuentas) (+ contador 1))
          contador)
      null))

(define (buscarCuenta_activar listCuenta cuenta password)
  (if (and (string? cuenta)
           (string? password)
           (Cuentas? listCuenta)
           (not (null? listCuenta)))
      (if (and (eqv? (getNombre_C (car listCuenta)) cuenta)
               (eqv? (getContrasena_C (car listCuenta)) password))
          (cons (account (getNombre_C (car listCuenta))
                      (getContrasena_C (car listCuenta))
                      (getFecha_C (car listCuenta))
                      #t
                      (getFollow_C (car listCuenta))
                      (getListFollow_C (car listCuenta))
                      (getID_C (car listCuenta))
                      (getListPublicaciones_C (car listCuenta)))
                (buscarCuenta_activar (cdr listCuenta) cuenta password))
          (cons (car listCuenta) (buscarCuenta_activar (cdr listCuenta) cuenta password)))
      null))

(define (activar SN cuenta password)
  (if (and (string? cuenta)
           (string? password)
           (socialnetwork? SN))
      (if (not (null? (buscarCuenta_activar (getCuenta_SN SN) cuenta password)))
          (list (getName_SN SN) (getDate_SN SN) (getEncryptFunction_SN SN) (getDecryptFunction_SN SN)
                (buscarCuenta_activar (getCuenta_SN SN) cuenta password))
          SN)
      SN))

(define (buscarCuenta_desactivar listCuenta)
  (if (and (Cuentas? listCuenta)
           (not (null? listCuenta)))
      (if (eqv? (getActividad_C (car listCuenta)) #t)
          (cons (account (getNombre_C (car listCuenta))
                      (getContrasena_C (car listCuenta))
                      (getFecha_C (car listCuenta))
                      #f
                      (getFollow_C (car listCuenta))
                      (getListFollow_C (car listCuenta))
                      (getID_C (car listCuenta))
                      (getListPublicaciones_C (car listCuenta)))
                (buscarCuenta_desactivar (cdr listCuenta)))
          (cons (car listCuenta) (buscarCuenta_desactivar (cdr listCuenta))))
      null))

(define (desactivar SN)
  (if (and (socialnetwork? SN))
      (if (not (null? (buscarCuenta_desactivar (getCuenta_SN SN))))
          (list (getName_SN SN) (getDate_SN SN) (getEncryptFunction_SN SN) (getDecryptFunction_SN SN)
                (buscarCuenta_desactivar (getCuenta_SN SN)))
          SN)
      SN))

(define (buscarCuentaActiva SN)
  (if (socialnetwork? SN)
      (buscarCuentaActiva_encaps (getCuenta_SN SN))
      null))

(define (buscarCuentaActiva_encaps listCuenta)
  (if (not (null? listCuenta))
      (if (eqv? (getActividad_C (car listCuenta)) #t)
          (car listCuenta)
          (buscarCuentaActiva_encaps (cdr listCuenta)))
      null))