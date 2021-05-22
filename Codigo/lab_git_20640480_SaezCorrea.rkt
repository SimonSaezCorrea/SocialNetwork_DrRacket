#lang racket

(require "date.rkt")
(require "Cuenta.rkt")
(require "Publicacion.rkt")
(require "socialnetwork.rkt")



;Funciones principales

#|
Des: Permite registrar una cuenta
Dom: La socialnetwork, la fecha, el nombre y la contraseña
Rec: La socialnetwork con la nueva cuenta
|#
(define (register socialnetwork date username password)
  (if (and (string? username)
           (string? password)
           (day? date)
           (socialnetwork? socialnetwork)
           (existeUsuario? socialnetwork username))
      (addCuenta_SN socialnetwork (account username password date #f 0 (list )
                                           (contadorCuentas (getCuenta_SN socialnetwork) 1) (list ) (list )))
      socialnetwork))

;##########################################################################################################################


#|
Des: Permite hacer login a una cuenta y hacer una funcion
Dom: La socialnetwork,  el nombre, la contraseña y la operacion | fecha | otro parametros
Rec: La socialnetwork con la nueva cuenta
|#

(define login (lambda (socialnetwork username password operation)(lambda (date)(lambda (parametro1 . parametro2)
    (if (and (string? username)
             (string? password)
             (socialnetwork? socialnetwork))
        (if (and (day? date)
                 (not (null? (cons parametro1 parametro2))))
            (((operation (activar socialnetwork username password)) date) parametro1 parametro2)
            socialnetwork)
        socialnetwork)))))


;##########################################################################################################################


#|
Des: Permite hacer un post de una cuenta logeada
Dom: El socialnetwork, la fecha y una lista con la publicacion y posibles usuarios de envio
Rec: La socialnetwork con el post puesto
|#


(define (post socialnetwork) (lambda (date) (lambda (resp users)
  (if (null? users)
      (postCuentaUsuario socialnetwork date resp)
      (postCuentaOtroUsuarios socialnetwork date resp users)))))

; °°°°°°°°°°°°°°°°°°°°°°°°
; PARA EL MISMO USUARIO

#|
Des: Permite hacer un post a su propia cuenta
Dom: La socialnetwork, la fecha y la publicacion
Rec: La socialnetwork con el post puesto
|#
(define (postCuentaUsuario socialnetwork date publicacion)
  (if (and (day? date)
           (string? publicacion)
           (socialnetwork? socialnetwork))
      (desactivar (list (getName_SN socialnetwork)
                        (getDate_SN socialnetwork)
                        (getEncryptFunction_SN socialnetwork)
                        (getDecryptFunction_SN socialnetwork)
                        (postCuentaUsuario_encaps (getCuenta_SN socialnetwork) date publicacion
                                                  (getPublicaciones_SN socialnetwork)(getEncryptFunction_SN socialnetwork))
                        (postListPublic (getPublicaciones_SN socialnetwork) (getPublicaciones_SN socialnetwork)
                                        date publicacion (getNombre_C (buscarCuentaActiva socialnetwork))
                                        (getEncryptFunction_SN socialnetwork))))
      ;(desactivar socialnetwork)))
      (list date publicacion)))

#|
Des: Permite hacer un post a su propia cuenta
Dom: La lista de cuentas, la fecha y la publicacion
Rec: La lista de cuentas con el post puesto
|#
(define (postCuentaUsuario_encaps listCuenta date publicacion listPublic encrypt)
  (if(not(null? listCuenta))
     (if(eqv? (getActividad_C (car listCuenta)) #t)
        (cons (addPublicacion (car listCuenta) (posting (getNombre_C (car listCuenta)) date "text" (encrypt publicacion)
                                                    (contadorPublicaciones listPublic 1)))
              (postCuentaUsuario_encaps (cdr listCuenta) date publicacion listPublic encrypt))
        (cons (car listCuenta) (postCuentaUsuario_encaps (cdr listCuenta) date publicacion listPublic encrypt)))
     null))


; °°°°°°°°°°°°°°°°°°°°°°°°
; PARA OTRO USUARIO


#|
Des: Permite hacer un post a cuentas de otros usuarios
Dom: La socialnetwork, la fecha, la publicacion y la lista de usuarios a la que va dirijido
Rec: La socialnetwork con el post puesto
|#
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
                                                       (buscarCuentaActiva socialnetwork)
                                                       (getPublicaciones_SN socialnetwork)
                                                       (getEncryptFunction_SN socialnetwork))
                        (postListPublic_otrosUsers (getPublicaciones_SN socialnetwork) (getPublicaciones_SN socialnetwork)
                                                   date publicacion listUsuario
                                                   (getNombre_C (buscarCuentaActiva socialnetwork))
                                                   (getEncryptFunction_SN socialnetwork))))
      (desactivar socialnetwork)))

#|
Des: Permite hacer un post a cuentas de otros usuarios
Dom: La lista de cuentas, la fecha, la publicacion, la lista de usuarios a la que va dirijido y la cuenta autora
Rec: La socialnetwork con el post puesto
|#
(define (postCuentaOtroUsuarios_encaps listCuentas date publicacion listUsuario cuentaAutora listPublic encrypt)
  (if (not (null? listUsuario))
      (postCuentaOtroUsuarios_encaps (postCuentaOtroUsuarios_encaps_2 listCuentas date publicacion listUsuario
                                                                      cuentaAutora listPublic encrypt)
                                     date publicacion (cdr listUsuario) cuentaAutora listPublic encrypt)
      listCuentas))

#|
Des: Permite hacer un post a cuentas de otros usuarios
Dom: La lista de cuentas, la fecha, la publicacion, la lista de usuarios a la que va dirijido y la cuenta autora
Rec: La socialnetwork con el post puesto
|#
(define (postCuentaOtroUsuarios_encaps_2 listCuentas date publicacion listUsuario cuentaAutora listPublic encrypt)
  (if (not (null? listCuentas))
      (if (eqv? (car listUsuario) (getNombre_C (car listCuentas)))
          (cons (addPublicacion (car listCuentas) (posting (getNombre_C cuentaAutora) date "text" (encrypt publicacion)
                                                           (contadorPublicaciones listPublic 1)))
                (postCuentaOtroUsuarios_encaps_2 (cdr listCuentas) date publicacion listUsuario cuentaAutora listPublic
                                                 encrypt))
          (cons (car listCuentas) (postCuentaOtroUsuarios_encaps_2 (cdr listCuentas) date publicacion listUsuario
                                                                   cuentaAutora listPublic encrypt)))
      null))

;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
;SE AÑADE A LA LISTA DE PUBLICACIONES QUE ESTAN TODAS
#|
Des: Permite añadir la publicacion a la lista de publicaciones general
Dom: La lista de publicacion, la fecha, la publicacion y el autor
Rec: La lista de publicaciones modificada
|#
(define (postListPublic listPublicaciones listPublicacionesTotal date publicacion autor encrypt)
  (if (not (null? listPublicaciones))
      (cons (car listPublicaciones) (postListPublic (cdr listPublicaciones) date publicacion autor))
      (cons (list (posting autor date "text" (encrypt publicacion) (contadorPublicaciones listPublicacionesTotal 1))
                  autor)
            null)))

#|
Des: Permite añadir la publicacion a la lista de publicaciones general
Dom: Lista de publicaciones, fecha, publicacion, lista de usuarios y el autor
Rec: La lista de publicaciones modificada
|#
(define (postListPublic_otrosUsers listPublicaciones listPublicacionesTotal date publicacion listUser autor encrypt)
  (if (not(null? listUser))
      (postListPublic_otrosUsers (postListPublic_otrosUsers_encaps listPublicaciones listPublicacionesTotal
                                                                   date publicacion listUser autor encrypt)
                                 (postListPublic_otrosUsers_encaps listPublicaciones listPublicacionesTotal
                                                                   date publicacion listUser autor encrypt)
                                 date publicacion (cdr listUser) autor encrypt)
      listPublicaciones))

#|
Des: Permite añadir la publicacion a la lista de publicaciones general
Dom: Lista de publicaciones, fecha, publicacion, lista de usuarios y el autor
Rec: La lista de publicaciones modificada
|#
(define (postListPublic_otrosUsers_encaps listPublicaciones listPublicacionesTotal date publicacion listUser autor encrypt)
  (if (not (null? listUser))
      (if (not (null? listPublicaciones))
          (cons (car listPublicaciones) (postListPublic_otrosUsers_encaps (cdr listPublicaciones) listPublicacionesTotal
                                                                          date publicacion listUser autor encrypt))
          (cons (list (posting autor date "text" (encrypt publicacion) (contadorPublicaciones listPublicacionesTotal 1))
                      (car listUser))
                null))
      null))

;##########################################################################################################################

#|
Des: Permite hacer un follow a un usuario
Dom: la socialnetwork, la fecha y un usuario (string)
Rec: La socialnetwork modificada
|#
(define (follow socialnetwork) (lambda (date) (lambda (usuario nada)
  (if (and (day? date)
           (string? usuario))
      (if (not (existeUsuario? socialnetwork usuario))
          (desactivar (list (getName_SN socialnetwork)
                            (getDate_SN socialnetwork)
                            (getEncryptFunction_SN socialnetwork)
                            (getDecryptFunction_SN socialnetwork)
                            (follow_encaps (getCuenta_SN socialnetwork) date usuario (buscarCuentaActiva socialnetwork))
                            (getPublicaciones_SN socialnetwork)))
          socialnetwork)
      socialnetwork))))

#|
Des: Permite hacer un follow a un usuario
Dom: la lista de usuarios, la fecha y un usuario (string)
Rec: La lista de usuarios modificada
|#
(define (follow_encaps listaUsuario date usuario usuarioActivo)
  (if (not(null? listaUsuario))
      (if (eqv? (getNombre_C (car listaUsuario)) usuario)
          (cons (addListFollow (car listaUsuario) (getNombre_C usuarioActivo)) (follow_encaps (cdr listaUsuario)
                                                                                              date usuario usuarioActivo))
          (cons (car listaUsuario) (follow_encaps (cdr listaUsuario) date usuario usuarioActivo)))
      null))


;##########################################################################################################################

#|
Des: Permite compartir una publicacion segun ID a si mismo o alguien
Dom: El socialnetwork, la fecha y el ID
Rec: La socialnetwork modificada
|#
(define (share socialnetwork) (lambda (date) (lambda (ID users)
  (if (null? users)
      (shareMiCuenta socialnetwork date ID)
      (shareOtraCuenta socialnetwork date ID users)))))

#|
Des: Permite compartir una publicacion segun ID a si mismo
Dom: El socialnetwork, la fecha y el ID
Rec: La socialnetwork modificada
|#
(define (shareMiCuenta SN date ID)
  (if (and (socialnetwork? SN)
           (day? date)
           (integer? ID))
      (desactivar (list (getName_SN SN)
                        (getDate_SN SN)
                        (getEncryptFunction_SN SN)
                        (getDecryptFunction_SN SN)
                        (shareMiCuenta_encaps (getCuenta_SN SN) (getPublicaciones_SN SN) date ID)
                        (getPublicaciones_SN SN)))
      SN))

#|
Des: Permite compartir una publicacion segun ID a si mismo
Dom: La lista de usuario, la lista de publicaciones, la fecha y el ID
Rec: La lista de usuarios modificada
|#
(define (shareMiCuenta_encaps listUsuarios listPublicacion date ID)
  (if (not (null? listUsuarios))
      (if (eqv? #t (getActividad_C (car listUsuarios)))
          (if (eqv? (getID_P (car (car listPublicacion))) ID)
              (cons (addPublicacionCompartidas (car listUsuarios) (car (car listPublicacion))
                                               (getNombre_C (buscarCuentaActiva_encaps listUsuarios)))
                    (shareMiCuenta_encaps (cdr listUsuarios) listPublicacion date ID))
              (shareMiCuenta_encaps listUsuarios (cdr listPublicacion) date ID))
          (cons (car listUsuarios) (shareMiCuenta_encaps (cdr listUsuarios) listPublicacion date ID)))
  null))

;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°

#|
Des: Permite compartir una publicacion segun ID a alguien
Dom: El socialnetwork, la fecha, el ID y la lista de usuarios a compartir
Rec: La socialnetwork modificada
|#
(define (shareOtraCuenta SN date ID listUser)
  (if (and (socialnetwork? SN)
           (day? date)
           (integer? ID))
      (desactivar (list (getName_SN SN)
                        (getDate_SN SN)
                        (getEncryptFunction_SN SN)
                        (getDecryptFunction_SN SN)
                        (shareOtraCuenta_encaps (getCuenta_SN SN) (getPublicaciones_SN SN) date ID listUser)
                        (getPublicaciones_SN SN)))
      SN))

#|
Des: Permite compartir una publicacion segun ID a alguien
Dom: La lista de usuarios, la lista de publicaciones, la fecha, el ID y la lista de usuarios a compartir
Rec: La lista de usuarios modificada
|#
(define (shareOtraCuenta_encaps listUsuarios listPublicacion date ID listUser)
  (if (not(null? listUser))
      (shareOtraCuenta_encaps (shareOtraCuenta_encaps2 listUsuarios listPublicacion date ID listUser
                                                       (getNombre_C (buscarCuentaActiva_encaps listUsuarios)))
                              listPublicacion date ID (cdr listUser))
      listUsuarios))

#|
Des: Permite compartir una publicacion segun ID a alguien
Dom: La lista de usuarios, la lista de publicaciones, la fecha, el ID, la lista de usuarios a compartir y el autor
Rec: La lista de usuarios modificada
|#
(define (shareOtraCuenta_encaps2 listUsuarios listPublicacion date ID listUser autor)
  (if (not(null? listUsuarios))
      (if (eqv? (getNombre_C (car listUsuarios)) (car listUser))
          (if (eqv? ID (getID_P (car (car listPublicacion))))
              (cons (addPublicacionCompartidas (car listUsuarios) (car (car listPublicacion)) autor)
                    (shareOtraCuenta_encaps2 (cdr listUsuarios) listPublicacion date ID listUser autor))
              (shareOtraCuenta_encaps2 listUsuarios (cdr listPublicacion) date ID listUser autor))
          (cons (car listUsuarios) (shareOtraCuenta_encaps2 (cdr listUsuarios) listPublicacion date ID listUser autor)))
      null))


;##########################################################################################################################

(define (socialnetwork->string socialnetwork) (lambda (nada1) (lambda (nada2 nada3)
                                                socialnetwork)))


;####################

;EJEMPLOS

(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))

(define emptyFB (socialnetwork "fb" (day 25 10 2021) encryptFn encryptFn))

(define FB (register (register (register emptyFB (day 25 10 2021) "user1" "pass1")
(day 25 10 2021) "user2" "pass2") (day 25 10 2021) "user3" "pass3"))


(define FB1 (((login FB "user1" "pass1" post) (day 28 10 2021)) "0st post"))

(define FB2 (((login FB1 "user3" "pass3" post) (day 28 10 2021)) "1th post" "user1"))

(define FB3 (((login FB2 "user1" "pass1" post) (day 28 10 2021)) "2th post" "user2" "user3"))


(define FB4 (((login FB3 "user1" "pass1" follow) (day 27 10 2021)) "user2"))

(define FB5 (((login FB4 "user1" "pass1" follow) (day 27 10 2021)) "user3"))

(define FB6 (((login FB5 "user2" "pass2" follow) (day 27 10 2021)) "user3"))

(define FB7 (((login FB6 "user2" "pass2" follow) (day 27 10 2021)) "user1"))

(define FB8 (((login FB7 "user1" "pass1" share) (day 28 10 2021)) 2))

(define FB9 (((login FB8 "user1" "pass1" share) (day 28 10 2021)) 2 "user2" "user3"))

(define FB10 (((login FB9 "user3" "pass3" share) (day 28 10 2021)) 4 "user2" "user1"))
