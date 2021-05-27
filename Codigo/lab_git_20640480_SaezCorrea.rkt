#lang racket

(require "date.rkt")
(require "Cuenta.rkt")
(require "Publicacion.rkt")
(require "socialnetwork.rkt")
(require "Comment.rkt")



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
Dom: La socialnetwork,  el nombre, la contraseña y la operacion
Rec: La socialnetwork modificado segun operacion
|#
(define (login socialnetwork username password operation)
    (if (and (string? username)
             (string? password)
             (socialnetwork? socialnetwork))
        (operation (activar socialnetwork username password))
        socialnetwork))


;##########################################################################################################################


#|
Des: Permite hacer un post de una cuenta logeada
Dom: El socialnetwork, la fecha, el tipo de contenido, una publicacion y lista de usuarios de envio
Rec: La socialnetwork con el post puesto
|#
(define (post socialnetwork) (lambda (date) (lambda (tipo resp . users)
  (if (null? users)
      (postCuentaUsuario socialnetwork date tipo resp)
      (postCuentaOtroUsuarios socialnetwork date tipo resp users)))))

; °°°°°°°°°°°°°°°°°°°°°°°°
; PARA EL MISMO USUARIO

#|
Des: Permite hacer un post a su propia cuenta
Dom: La socialnetwork, la fecha y la publicacion
Rec: La socialnetwork con el post puesto
|#
(define (postCuentaUsuario socialnetwork date tipo publicacion)
  (if (and (day? date)
           (string? publicacion)
           (socialnetwork? socialnetwork))
      (desactivar (list (getName_SN socialnetwork)
                        (getDate_SN socialnetwork)
                        (getEncryptFunction_SN socialnetwork)
                        (getDecryptFunction_SN socialnetwork)
                        (postCuentaUsuario_encaps (getCuenta_SN socialnetwork) date tipo publicacion
                                                  (getPublicaciones_SN socialnetwork)(getEncryptFunction_SN socialnetwork))
                        (postListPublic (getPublicaciones_SN socialnetwork)
                                        (getPublicaciones_SN socialnetwork)
                                        date
                                        tipo
                                        publicacion
                                        (getNombre_C (buscarCuentaActiva socialnetwork))
                                        (getEncryptFunction_SN socialnetwork)
                                        (list (getNombre_C (buscarCuentaActiva socialnetwork))))
                        (getComentarios_SN socialnetwork)))
      (desactivar socialnetwork)))

#|
Des: Permite hacer un post a su propia cuenta
Dom: La lista de cuentas, la fecha, la publicacion y el encrypt
Rec: La lista de cuentas con el post puesto
|#
(define (postCuentaUsuario_encaps listCuenta date tipo publicacion listPublic encrypt)
  (if(not(null? listCuenta))
     (if(eqv? (getActividad_C (car listCuenta)) #t)
        (cons (addPublicacion (car listCuenta) (posting (getNombre_C (car listCuenta)) date tipo (encrypt publicacion)
                                                    (contadorPublicaciones listPublic 1) (list) 0 0))
              (postCuentaUsuario_encaps (cdr listCuenta) date tipo publicacion listPublic encrypt))
        (cons (car listCuenta) (postCuentaUsuario_encaps (cdr listCuenta) date tipo publicacion listPublic encrypt)))
     null))


; °°°°°°°°°°°°°°°°°°°°°°°°
; PARA OTRO USUARIO


#|
Des: Permite hacer un post a cuentas de otros usuarios
Dom: La socialnetwork, la fecha, la publicacion y la lista de usuarios a la que va dirijido
Rec: La socialnetwork con el post puesto
|#
(define (postCuentaOtroUsuarios socialnetwork date tipo publicacion listUsuario)
  (if (and (day? date)
           (string? publicacion)
           (not (null? listUsuario))
           (socialnetwork? socialnetwork))
      (desactivar (list (getName_SN socialnetwork)
                        (getDate_SN socialnetwork)
                        (getEncryptFunction_SN socialnetwork)
                        (getDecryptFunction_SN socialnetwork)
                        (postCuentaOtroUsuarios_encaps (getCuenta_SN socialnetwork) date tipo publicacion listUsuario
                                                       (buscarCuentaActiva socialnetwork)
                                                       (getPublicaciones_SN socialnetwork)
                                                       (getEncryptFunction_SN socialnetwork))
                        (postListPublic (getPublicaciones_SN socialnetwork) (getPublicaciones_SN socialnetwork)
                                        date tipo publicacion (getNombre_C (buscarCuentaActiva socialnetwork))
                                        (getEncryptFunction_SN socialnetwork) listUsuario)
                        (getComentarios_SN socialnetwork)))
      (desactivar socialnetwork)))

#|
Des: Permite hacer un post a cuentas de otros usuarios
Dom: La lista de cuentas, la fecha, la publicacion, la lista de usuarios a la que va dirijido, la cuenta autora,
     listaDePost y encrypt
Rec: La socialnetwork con el post puesto
|#
(define (postCuentaOtroUsuarios_encaps listCuentas date tipo publicacion listUsuario cuentaAutora listPublic encrypt)
  (if (not (null? listUsuario))
      (postCuentaOtroUsuarios_encaps (postCuentaOtroUsuarios_encaps_2 listCuentas date tipo publicacion listUsuario
                                                                      cuentaAutora listPublic encrypt)
                                     date tipo publicacion (cdr listUsuario) cuentaAutora listPublic encrypt)
      listCuentas))

#|
Des: Permite hacer un post a cuentas de otros usuarios
Dom: La lista de cuentas, la fecha, la publicacion, la lista de usuarios a la que va dirijido, la cuenta autora,
     listaDePost y encrypt
Rec: La socialnetwork con el post puesto
|#
(define (postCuentaOtroUsuarios_encaps_2 listCuentas date tipo publicacion listUsuario cuentaAutora listPublic encrypt)
  (if (not (null? listCuentas))
      (if (eqv? (car listUsuario) (getNombre_C (car listCuentas)))
          (cons (addPublicacion (car listCuentas) (posting (getNombre_C cuentaAutora) date "text" (encrypt publicacion)
                                                           (contadorPublicaciones listPublic 1) (list) 0 0) )
                (postCuentaOtroUsuarios_encaps_2 (cdr listCuentas) date tipo publicacion listUsuario cuentaAutora
                                                 listPublic encrypt))
          (cons (car listCuentas) (postCuentaOtroUsuarios_encaps_2 (cdr listCuentas) date tipo publicacion listUsuario
                                                                   cuentaAutora listPublic encrypt)))
      null))

;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
;SE AÑADE A LA LISTA DE PUBLICACIONES QUE ESTAN TODAS
#|
Des: Permite añadir la publicacion a la lista de publicaciones general
Dom: La lista de publicacion, la fecha, la publicacion, el autor y el encrypt
Rec: La lista de publicaciones modificada
|#
(define (postListPublic listPublicaciones listPublicacionesTotal date tipo publicacion autor encrypt listUser)
  (if (not (null? listPublicaciones))
      (cons (car listPublicaciones) (postListPublic (cdr listPublicaciones) listPublicacionesTotal date tipo publicacion
                                                    autor encrypt listUser))
      (cons (list (posting autor date tipo (encrypt publicacion) (contadorPublicaciones listPublicacionesTotal 1) (list) 0 0)
                  listUser)
            null)))


;##########################################################################################################################

#|
Des: Permite hacer un follow a un usuario
Dom: la socialnetwork, la fecha y un usuario (string)
Rec: La socialnetwork modificada
|#
(define (follow socialnetwork) (lambda (date) (lambda (usuario)
  (if (existeUserActivo? socialnetwork)
      (if (and (day? date)
               (string? usuario))
          (if (not (existeUsuario? socialnetwork usuario))
              (desactivar (list (getName_SN socialnetwork)
                                (getDate_SN socialnetwork)
                                (getEncryptFunction_SN socialnetwork)
                                (getDecryptFunction_SN socialnetwork)
                                (follow_encaps (getCuenta_SN socialnetwork) date usuario (buscarCuentaActiva socialnetwork))
                                (getPublicaciones_SN socialnetwork)
                                (getComentarios_SN socialnetwork)))
              (desactivar socialnetwork))
          (desactivar socialnetwork))
      socialnetwork))))

#|
Des: Permite hacer un follow a un usuario
Dom: la lista de usuarios, la fecha y un usuario (string)
Rec: La lista de usuarios modificada
|#
(define (follow_encaps listaUsuario date usuario usuarioActivo)
  (if (not(null? listaUsuario))
      (if (eqv? (getNombre_C (car listaUsuario)) usuario)
          (cons (addListFollow (car listaUsuario) (getNombre_C usuarioActivo) date) (follow_encaps (cdr listaUsuario)
                                                                                              date usuario usuarioActivo))
          (cons (car listaUsuario) (follow_encaps (cdr listaUsuario) date usuario usuarioActivo)))
      null))


;##########################################################################################################################

#|
Des: Permite compartir una publicacion segun ID a si mismo o alguien
Dom: El socialnetwork, la fecha, el ID y la lista de users||
Rec: La socialnetwork modificada
|#
(define (share socialnetwork) (lambda (date) (lambda (ID . users)
  (if (existeUserActivo? socialnetwork)
      (if (null? users)
          (shareMiCuenta socialnetwork date ID)
          (shareOtraCuenta socialnetwork date ID users))
      (desactivar socialnetwork)))))

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
                        (getPublicaciones_SN SN)
                        (getComentarios_SN SN)))
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
                                               (getNombre_C (buscarCuentaActiva_encaps listUsuarios)) date)
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
                        (getPublicaciones_SN SN)
                        (getComentarios_SN SN)))
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
              (cons (addPublicacionCompartidas (car listUsuarios) (car (car listPublicacion)) autor date)
                    (shareOtraCuenta_encaps2 (cdr listUsuarios) listPublicacion date ID listUser autor))
              (shareOtraCuenta_encaps2 listUsuarios (cdr listPublicacion) date ID listUser autor))
          (cons (car listUsuarios) (shareOtraCuenta_encaps2 (cdr listUsuarios) listPublicacion date ID listUser autor)))
      null))


;##########################################################################################################################

#|
Des: Permite pasar a string el contenido de un usuario o de todo el socialnetwork
Dom: Socialnetwork
Rec: String
|#
(define (socialnetwork->string socialnetwork)
  (if (existeUserActivo? socialnetwork)
      (social->string_login (buscarCuentaActiva socialnetwork) (getDecryptFunction_SN socialnetwork))
      (social->string_total socialnetwork (getDecryptFunction_SN socialnetwork))))

#|
Des: Permite pasar a string el contenido de un usuario
Dom: Usuario y descriptacion
Rec: String
|#
(define (social->string_login User descrypt)
  (string->users User descrypt "activada"))

#|
Des: Permite pasar a string el contenido de todo el socialnetwork
Dom: Socialnetwork y descriptacion
Rec: String
|#
(define (social->string_total socialnetwork descrypt)
  (string-append "El nombre de la socialnetowrk es " (getName_SN socialnetwork) "\n"
                 "Las cuentas que tiene son:\n"
                 (string->Cuentas (getCuenta_SN socialnetwork) descrypt "")
                 "\n°°°°°°°°°°°°°°°°°°°°°°°°°°°°°\n"
                 "Las publicaciones son:\n"
                 (string->Publicaciones (getPublicaciones_SN socialnetwork) descrypt "")))

;###########################################################################################################################

(define (comment socialnetwork) (lambda (date) (lambda (ID) (lambda (respuesta)
  (if(and(day? date)
         (integer? ID)
         (string? respuesta))
     (desactivar (list(getName_SN socialnetwork)
                      (getDate_SN socialnetwork)
                      (getEncryptFunction_SN socialnetwork)
                      (getDecryptFunction_SN socialnetwork)
                      (getCuenta_SN socialnetwork)
                      (comment_encaps (getPublicaciones_SN socialnetwork) date ID
                                      ((getEncryptFunction_SN socialnetwork)respuesta))
                      (anadirComent (getComentarios_SN socialnetwork)
                                    (comentario (contadorComentarios (getComentarios_SN socialnetwork) -1)
                                                (getNombre_C (buscarCuentaActiva socialnetwork))
                                                respuesta
                                                date
                                                0))))
     socialnetwork)))))

(define (comment_encaps listPub date ID resp)
  (if (not(null? listPub))
      (if (eq? (getID_P (car (car listPub))) ID)
          (cons (list (addComentario (car (car listPub)) resp) (car (cdr (car listPub))))
                (comment_encaps (cdr listPub) date ID resp))
          (cons (car listPub) (comment_encaps (cdr listPub) date ID resp)))
      null))

(define (anadirComent listComent comment)
  (if (not (null? listComent))
      (cons (car listComent) (anadirComent (cdr listComent) comment))
      (cons comment null)))
  
;###########################################################################################################################

(define (like socialnetwork) (lambda (date) (lambda (ID)
  (if (and (day? date)
           (integer? ID))
      (if (> ID 0)
          (desactivar (list (getName_SN socialnetwork)
                            (getDate_SN socialnetwork)
                            (getEncryptFunction_SN socialnetwork)
                            (getDecryptFunction_SN socialnetwork)
                            (getCuenta_SN socialnetwork)
                            (like_Post (getPublicaciones_SN socialnetwork) ID)
                            (getComentarios_SN socialnetwork)))
          (desactivar (list (getName_SN socialnetwork)
                            (getDate_SN socialnetwork)
                            (getEncryptFunction_SN socialnetwork)
                            (getDecryptFunction_SN socialnetwork)
                            (getCuenta_SN socialnetwork)
                            (getPublicaciones_SN socialnetwork)
                            (like_Coment (getComentarios_SN socialnetwork) ID))))
      socialnetwork))))

(define (like_Post listPost ID)
  (if(not(null? listPost))
     (if (eq? (getID_P (car (car listPost))) ID)
         (cons (list (list (getAutor_P (car (car listPost)))
                           (getFecha_P (car (car listPost)))
                           (getTipo_P (car (car listPost)))
                           (getContenido_P (car (car listPost)))
                           (getID_P (car (car listPost)))
                           (getComentario_P (car (car listPost)))
                           (+ (getLikesP_P (car (car listPost))) 1)
                           (getCantComp_P (car (car listPost))))
                     (car(cdr(car listPost))))
               (like_Post (cdr listPost) ID))
         (cons (car listPost) (like_Post (cdr listPost) ID)))
     null))

(define (like_Coment listComent ID)
  (if(not(null? listComent))
     (if (eq? (getID_Comment (car listComent)) ID)
         (cons (list (getID_Comment (car listComent))
                     (getAutor_Comment (car listComent))
                     (getComment_Comment (car listComent))
                     (getDate_Comment (car listComent))
                     (+ (getLikes_Comment (car listComent)) 1 ))
               (like_Coment (cdr listComent) ID))
         (cons (car listComent) (like_Coment (cdr listComent) ID)))
     null))

;###########################################################################################################################

;EJEMPLOS

#|

(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))

(define emptyFB (socialnetwork "fb" (day 25 10 2021) encryptFn encryptFn))

(define FB (register (register (register (register (register emptyFB (day 25 10 2021) "user1" "pass1")
                                                   (day 25 10 2021) "user2" "pass2")
                                         (day 25 10 2021) "user3" "pass3")
                               (day 25 10 2021) "user4" "pass4")
                     (day 25 10 2021) "user5" "pass5"))


(define FB1 (((login FB "user1" "pass1" post) (day 28 10 2021)) "text" "0st post"))

(define FB2 (((login FB1 "user2" "pass2" post) (day 28 10 2021)) "text" "1th post" "user1"))

(define FB3 (((login FB2 "user3" "pass3" post) (day 28 10 2021)) "text" "2th post" "user2" "user3"))

(define FB4 (((login FB3 "user4" "pass4" post) (day 29 10 2021)) "photo" "meme.png"))

(define FB5 (((login FB4 "user5" "pass5" post) (day 29 10 2021)) "video" "vidaFantastica.mp4" "user2" "user5"))

(define FB6 (((login FB5 "user1" "pass1" follow) (day 29 10 2021)) "user2"))

(define FB7 (((login FB6 "user2" "pass2" follow) (day 29 10 2021)) "user3"))

(define FB8 (((login FB7 "user3" "pass3" follow) (day 30 10 2021)) "user3"))

(define FB9 (((login FB8 "user4" "pass4" follow) (day 30 10 2021)) "user1"))

(define FB10 (((login FB9 "user5" "pass5" follow) (day 1 11 2021)) "user5"))

(define FB11 (((login FB10 "user1" "pass1" share) (day 5 11 2021)) 2))

(define FB12 (((login FB11 "user1" "pass1" share) (day 5 11 2021)) 2 "user2" "user3"))

(define FB13 (((login FB12 "user3" "pass3" share) (day 6 11 2021)) 3 "user2" "user1" "user4"))

(define FB14((((login FB13 "user1" "pass1" comment)(day 9 11 2021)) 2) "Este comentario 1"))

(define FB15((((login FB14 "user3" "pass3" comment)(day 12 11 2021)) 1) "Este comentario 2"))

(define FB16((((login FB15 "user3" "pass3" comment)(day 15 11 2021)) 1) "Este comentario 3"))

(define FB17(((login FB16 "user2" "pass2" like) (day 23 11 2021)) 3))

(define FB18(((login FB17 "user3" "pass3" like) (day 28 11 2021)) 3))

(define FB19(((login FB18 "user5" "pass5" like) (day 28 11 2021)) 2))

(define FB20(((login FB19 "user1" "pass1" like) (day 3 12 2021)) -1))

(define S1 (login FB20 "user1" "pass1" socialnetwork->string))
(define S2 (login FB20 "user2" "pass2" socialnetwork->string))
(define S3 (login FB20 "user3" "pass3" socialnetwork->string))
(define S4 (socialnetwork->string FB20))

|#