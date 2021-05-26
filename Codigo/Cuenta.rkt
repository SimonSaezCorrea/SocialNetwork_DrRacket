#lang racket

(require "date.rkt")
(require "Publicacion.rkt")
(provide (all-defined-out))


#|
Cuenta -> nombre | contrasena | fecha creacion | actividad | follows | list_follows | ID | listPublicaciones | lista par publicacionCompartida y usuario
          String | String | Date | Boolean | Integer | list account | Integer | list post | list list de publicacion y usuario
|#

;Constructor

#|
Des: Permite la creacion de un constructor de tipo account
Dom: nombre (String), contrasena (String), fecha (day), actividad (boolean), follow (Integer),
     listFollow (list cuentas (String)), ID (Integer), lista de publicaciones (list de posting)
Rec: Una lista con los datos
|#
(define (account nombre contrasena fecha actividad follow listFollow ID listPublicaciones listCompartida)
  (if (and (string? nombre)
           (string? contrasena)
           (day? fecha)
           (boolean? actividad)
           (integer? follow)
           (integer? ID))
      (list nombre contrasena fecha actividad follow listFollow ID listPublicaciones listCompartida)
      null))

;Selector

#|
Des: Permite obtener el dato de nombre
Dom: El dato account(Una lista)
Rec: El dato nombre
|#
(define (getNombre_C cuenta)
  (car cuenta))

#|
Des: Permite obtener el dato de contrasena
Dom: El dato account(Una lista)
Rec: El dato contrasena
|#
(define (getContrasena_C cuenta)
  (car (cdr cuenta)))

#|
Des: Permite obtener el dato de day
Dom: El dato account(Una lista)
Rec: El dato day
|#
(define (getFecha_C cuenta)
  (car (cdr (cdr cuenta))))

#|
Des: Permite obtener el dato de actividad
Dom: El dato account(Una lista)
Rec: El dato actividad
|#
(define (getActividad_C cuenta)
  (car (cdr (cdr (cdr cuenta)))))

#|
Des: Permite obtener el dato de follow
Dom: El dato account(Una lista)
Rec: El dato follow
|#
(define (getFollow_C cuenta)
  (car (cdr (cdr (cdr (cdr cuenta))))))

#|
Des: Permite obtener el dato de listaFollow
Dom: El dato account(Una lista)
Rec: El dato listaFollow
|#
(define (getListFollow_C cuenta)
  (car (cdr (cdr (cdr (cdr (cdr cuenta)))))))

#|
Des: Permite obtener el dato de ID
Dom: El dato account(Una lista)
Rec: El dato ID
|#
(define (getID_C cuenta)
  (car (cdr (cdr (cdr (cdr (cdr (cdr cuenta))))))))

#|
Des: Permite obtener el dato de lista de publicaciones
Dom: El dato account(Una lista)
Rec: El dato lista de publicaciones
|#
(define (getListPublicaciones_C cuenta)
  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr cuenta)))))))))

#|
Des: Permite obtener el dato de lista de publicaciones compartidas
Dom: El dato account(Una lista)
Rec: El dato lista de publicaciones compartidas
|#
(define (getListPublicacionesCompartidas_C cuenta)
  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr cuenta))))))))))



;Pertenencia

#|
Des: Permite saber si es de tipo account
Dom: una lista
Rec: sentencia booleana
|#
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

#|
Des: Permite cambiar la actividad de la cuenta
Dom: La cuenta (list) y la actividad (boolean)
Rec: La cuenta modificada
|#
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
                   (getListPublicaciones_C cuenta)
                   (getListPublicacionesCompartidas_C cuenta))
          cuenta)
      null))

;###########################################################

#|
Des: Permite añadir un nombre a la lista de followers de la cuenta
Dom: La cuenta que se le agregara y el nombre a agregar
Rec: La cuenta modificada
|#
(define (addListFollow cuenta nombre date)
  (if (and (account? cuenta)
           (string? nombre))
      (account (getNombre_C cuenta)
               (getContrasena_C cuenta)
               (getFecha_C cuenta)
               (getActividad_C cuenta)
               (+(getFollow_C cuenta) 1)
               (addListFollow_encaps (getListFollow_C cuenta) nombre date)
               (getID_C cuenta)
               (getListPublicaciones_C cuenta)
               (getListPublicacionesCompartidas_C cuenta))
      cuenta))

#|
Des: Permite añadir el nombre a la lista
Dom: lista de followers, el nombre a agregar
Rec: La lista de followers modificada
|#
(define (addListFollow_encaps listCuenta nombre date)
  (if (not(null? listCuenta))
      (cons (car listCuenta) (addListFollow_encaps (cdr listCuenta) nombre date))
      (cons (list nombre date) null)))

;###########################################################

#|
Des: Permite añadir una publicacion a la lista de publicaciones de una cuenta
Dom: La cuenta que se le agregara y la publicacion a agregar
Rec: Una nueva cuenta modificada
|#
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
              (addPublicacion_encaps (getListPublicaciones_C cuenta) publicacion)
              (getListPublicacionesCompartidas_C cuenta))
     cuenta))

#|
Des: Permite añadir una publicacion a la lista de publicaciones
Dom: La lista de publicaciones y la publicacion
Rec: La lista de publicaciones modificada
|#
(define (addPublicacion_encaps Publicaciones publicacion)
  (if (not(null? Publicaciones))
      (cons (car Publicaciones) (addPublicacion_encaps (cdr Publicaciones) publicacion))
      (cons (list (getAutor_P publicacion) (getContenido_P publicacion)) null)))

;####################################################################

#|
Des: Permite añadir una publicacion a la lista de publicaciones compartidas de una cuenta
Dom: La cuenta que se le agregara y la publicacion a agregar
Rec: Una nueva cuenta modificada
|#
(define (addPublicacionCompartidas cuenta publicacion name date)
  (if(and(account? cuenta)
         (post? publicacion))
     (account (getNombre_C cuenta)
              (getContrasena_C cuenta)
              (getFecha_C cuenta)
              (getActividad_C cuenta)
              (getFollow_C cuenta)
              (getListFollow_C cuenta)
              (getID_C cuenta)
              (getListPublicaciones_C cuenta)
              (addPublicacionCompartidas_encaps (getListPublicacionesCompartidas_C cuenta) publicacion name date))
     cuenta))

#|
Des: Permite añadir una publicacion a la lista de publicaciones compartidas
Dom: La lista de publicaciones y la publicacion
Rec: La lista de publicaciones modificada
|#
(define (addPublicacionCompartidas_encaps PublicacionesCompartidas publicacion name date)
  (if (not(null? PublicacionesCompartidas))
      (cons (car PublicacionesCompartidas) (addPublicacionCompartidas_encaps (cdr PublicacionesCompartidas) publicacion name date))
      (cons (list (list (getAutor_P publicacion) (getContenido_P publicacion)) name date) null)))

;#####################################################################################################################

#|
Des: Permite crear un string con el contenido de una lista de follows
Dom: La lista de follows y un string
Rec: String
|#
(define (string->listFollow listFollow string)
  (if (not (null? listFollow))
      (string->listFollow (cdr listFollow) (string-append string "     - " (car (car listFollow)) "\n"))
      string))

#|
Des: Permite crear un string con el contenido de una lista de publicaciones
Dom: La lista de post, un string y la funcion de descriptacion
Rec: String
|#
(define (string->listPost listDePost string descrypt)
  (if (not (null? listDePost))
      (string->listPost (cdr listDePost) (string-append string "-------------\n"
                                                               "     Contenido: " (descrypt (car(cdr(car listDePost)))) "\n"
                                                               "     Autor: " (car(car listDePost)) "\n") descrypt)
      string))

#|
Des: Permite crear un string con el contenido de una lista de publicacion compartida
Dom: La lista de post compartida, un string y la funcion de descriptacion
Rec: String
|#
(define (string->listPostComp listDePostComp string descrypt)
  (if (not(null? listDePostComp))
      (string->listPostComp (cdr listDePostComp)
                            (string-append string "-------------\n"
                                                  "      Compartida por el user: " (car(cdr(car listDePostComp))) "\n"
                                                  "      Contenido: " (descrypt (car(cdr(car(car listDePostComp))))) "\n"
                                                  "      Autor: " (car(car(car listDePostComp))) "\n")
                                                                                                                descrypt)
      string))

#|
Des: Permite crear un string con el contenido de un usuario
Dom: El user, la funcion de descriptacion y su actividad
Rec: String
|#
(define (string->users User descrypt actividad)
  (string-append "El ID del usuario es: " (number->string (getID_C User)) "\n"
                 "El nombre del usuario es: "(getNombre_C User) "\n"
                 "La contraseña es: " (getContrasena_C User) "\n"
                 (string->fecha (getFecha_C User))
                 "La sesion se encuentra " actividad "\n"
                 "Tiene una cantidad de " (number->string (getFollow_C User)) " Follows\n\n"
                 (string->listFollow (getListFollow_C User) "Los usuarios que te siguen son:\n") "\n"
                 (string->listPost (getListPublicaciones_C User) "Los post que tiene son:\n" descrypt) "\n"
                 "Los post compartidos que han hecho son:\n" (string->listPostComp (getListPublicacionesCompartidas_C User) ""
                                       descrypt)))