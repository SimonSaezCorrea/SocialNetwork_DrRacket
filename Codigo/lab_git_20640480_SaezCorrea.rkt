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
      (addCuenta_SN socialnetwork (account username password date #f 0 (list )
                                           (contadorCuentas (getCuenta_SN socialnetwork) 1) (list )))
      socialnetwork))


(define login (lambda (socialnetwork username password operation)(lambda (date)(lambda (resp . parametro2)
    (if (and (string? username)
             (string? password)
             (socialnetwork? socialnetwork))
        (if (and (day? date)
                 (not (null? (cons resp parametro2))))
            (operation (activar socialnetwork username password) date (cons resp parametro2))
            socialnetwork)
        socialnetwork)))))

(define (post socialnetwork date publicacion)
  (if (null? (cdr publicacion))
      (postCuentaUsuario socialnetwork date (car publicacion))
      (postCuentaOtroUsuarios socialnetwork date (car publicacion) (cdr publicacion))))

(define (postCuentaUsuario socialnetwork date publicacion)
  (if (and (day? date)
           (string? publicacion)
           (socialnetwork? socialnetwork))
      (list (getName_SN socialnetwork)
            (getDate_SN socialnetwork)
            (getEncryptFunction_SN socialnetwork)
            (getDecryptFunction_SN socialnetwork)
            (postCuentaUsuario_encaps (getCuenta_SN socialnetwork) date publicacion))
      socialnetwork))

(define (postCuentaUsuario_encaps listCuenta date publicacion)
  (if(not(null? listCuenta))
     (if(eqv? (getActividad_C (car listCuenta)) #t)
        (cons (addPublicacion (car listCuenta) (posting (car listCuenta) date "text" publicacion 0
                                                    (contadorPublicaciones (getListPublicaciones_C (car listCuenta)) 1)))
              (postCuentaUsuario_encaps (cdr listCuenta) date publicacion))
        (cons (car listCuenta) (postCuentaUsuario_encaps (cdr listCuenta) date publicacion)))
     null))


(define (postCuentaOtroUsuarios socialnetwork date publicacion listUsuario)
  (if (and (day? date)
           (string? publicacion)
           (not (null? listUsuario))
           (socialnetwork? socialnetwork))
      (list (getName_SN socialnetwork)
            (getDate_SN socialnetwork)
            (getEncryptFunction_SN socialnetwork)
            (getDecryptFunction_SN socialnetwork)
            (postCuentaOtroUsuarios_encaps (getCuenta_SN socialnetwork) (getCuenta_SN socialnetwork)
                                           date publicacion listUsuario (buscarCuentaActiva socialnetwork)))
      socialnetwork))

(define (postCuentaOtroUsuarios_encaps listCuentasTotal listCuentas date publicacion listUsuario cuentaAutora)
  (if (not (null? listUsuario))
      (if (not (null? listCuentas))
          (if (eqv? (car listUsuario) (getNombre_C (car listCuentas)))
              (cons (addPublicacion (car listCuentas) (posting cuentaAutora date "text" publicacion 0
                                                      (contadorPublicaciones (getListPublicaciones_C (car listCuentas)) 1)))
                    (postCuentaOtroUsuarios_encaps listCuentasTotal (cdr listCuentas) date publicacion listUsuario cuentaAutora))
              (cons (car listCuentas) (postCuentaOtroUsuarios_encaps listCuentasTotal (cdr listCuentas) date publicacion listUsuario cuentaAutora)))
          (postCuentaOtroUsuarios_encaps listCuentasTotal listCuentasTotal date publicacion (cdr listUsuario) cuentaAutora))
      null))
      