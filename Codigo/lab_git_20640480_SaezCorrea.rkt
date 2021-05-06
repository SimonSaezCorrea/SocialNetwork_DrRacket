#lang racket

(require "date.rkt")
(require "Cuenta.rkt")
(require "Publicacion.rkt")
(require "socialnetwork.rkt")


;Funciones principales

(define (register socialnetwork date username password)
  (if (and (string? username)
           (string? password)
           (day? date)
           (socialnetwork? socialnetwork))
      (addCuenta_SN socialnetwork (account username password date #f 0 (contadorCuentas (getCuenta_SN socialnetwork) 1)))
      socialnetwork))