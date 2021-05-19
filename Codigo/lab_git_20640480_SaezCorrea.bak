#lang racket

(require "date.rkt")
(require "Cuenta.rkt")
(require "Publicacion.rkt")
(require "socialnetwork.rkt")

#| Ejemplos

(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))

(define emptyFB (socialnetwork "fb" (day 25 10 2021) encryptFn encryptFn))

(define FB (register (register (register emptyFB (day 25 10 2021) "user1" "pass1")
(day 25 10 2021) "user2" "pass2") (day 25 10 2021) "user3" "pass3"))

(define FB1 (((login FB1 "user1" "pass1" post) (day 28 10 2021)) "0st post"))

(define FB2 (((login FB2 "user3" "pass3" post) (day 28 10 2021)) "1th post" "user1"))

(define FB3 (((login FB3 "user1" "pass1" post) (day 28 10 2021)) "2th post" "user2" "user3"))

|#

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
      (desactivar (list (getName_SN socialnetwork)
                        (getDate_SN socialnetwork)
                        (getEncryptFunction_SN socialnetwork)
                        (getDecryptFunction_SN socialnetwork)
                        (postCuentaUsuario_encaps (getCuenta_SN socialnetwork) date publicacion)))
      (desactivar socialnetwork)))

(define (postCuentaUsuario_encaps listCuenta date publicacion)
  (if(not(null? listCuenta))
     (if(eqv? (getActividad_C (car listCuenta)) #t)
        (cons (addPublicacion (car listCuenta) (posting (getNombre_C (car listCuenta)) date "text" publicacion 0
                                                    (contadorPublicaciones (getListPublicaciones_C (car listCuenta)) 1)))
              (postCuentaUsuario_encaps (cdr listCuenta) date publicacion))
        (cons (car listCuenta) (postCuentaUsuario_encaps (cdr listCuenta) date publicacion)))
     null))


(define (postCuentaOtroUsuarios socialnetwork date publicacion listUsuario)
  (if (and (day? date)
           (string? publicacion)
           (not (null? listUsuario))
           (socialnetwork? socialnetwork))
      (desactivar (list (getName_SN socialnetwork)
            (getDate_SN socialnetwork)
            (getEncryptFunction_SN socialnetwork)
            (getDecryptFunction_SN socialnetwork)
            (postCuentaOtroUsuarios_encaps (getCuenta_SN socialnetwork) date publicacion listUsuario
                                           (buscarCuentaActiva socialnetwork))))
      (desactivar socialnetwork)))

(define (postCuentaOtroUsuarios_encaps listCuentas date publicacion listUsuario cuentaAutora)
  (if (not (null? listUsuario))
      (postCuentaOtroUsuarios_encaps (postCuentaOtroUsuarios_encaps_2 listCuentas date publicacion listUsuario cuentaAutora)
                                     date publicacion (cdr listUsuario) cuentaAutora)
      listCuentas))

(define (postCuentaOtroUsuarios_encaps_2 listCuentas date publicacion listUsuario cuentaAutora)
  (if (not (null? listCuentas))
      (if (eqv? (car listUsuario) (getNombre_C (car listCuentas)))
          (cons (addPublicacion (car listCuentas) (posting (getNombre_C cuentaAutora) date "text" publicacion 0
                                                           (contadorPublicaciones (getListPublicaciones_C (car listCuentas)) 1)))
                (postCuentaOtroUsuarios_encaps_2 (cdr listCuentas) date publicacion listUsuario cuentaAutora))
          (cons (car listCuentas) (postCuentaOtroUsuarios_encaps_2 (cdr listCuentas) date publicacion listUsuario cuentaAutora)))
      null))