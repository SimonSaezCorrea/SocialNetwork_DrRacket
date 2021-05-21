#lang racket

(require "date.rkt")
(require "Publicacion.rkt")
(provide (all-defined-out))
#|
(provide getNombre_C)
(provide getContrasena_C)
(provide getFecha_C)
(provide getActividad_C)
(provide getFollow_C)
(provide getListFollow_C)
(provide getID_C)
(provide getListPublicaciones_C)
(provide getListPublicacionesCompartidas_C)

(provide account)

(provide account?)
(provide Publicaciones?)

(provide setActividad)
(provide addListFollow)
(provide addPublicacion)
(provide addPublicacion_encaps)
(provide addPublicacionCompartidas)
|#


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
                          (if (Publicaciones? (getListPublicaciones_C cuenta))
                              #t
                              #f)
                          #f)
                      #f)
                  #f)
              #f)
          #f)
      #f))

#|
Des: Permite saber si la lista de publicaciones contiene publicaciones
Dom: Una lista
Rec: Sentencia booleana
|#
(define (Publicaciones? Publicaciones)
  (if (not(null? Publicaciones))
      (if (post? (car Publicaciones))
          (Publicaciones? (cdr Publicaciones))
          #f)
      #t))


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
(define (addListFollow cuenta nombre)
  (if (and (account? cuenta)
           (string? nombre))
      (account (getNombre_C cuenta)
               (getContrasena_C cuenta)
               (getFecha_C cuenta)
               (getActividad_C cuenta)
               (+(getFollow_C cuenta) 1)
               (addListFollow_encaps (getListFollow_C cuenta) nombre)
               (getID_C cuenta)
               (getListPublicaciones_C cuenta)
               (getListPublicacionesCompartidas_C cuenta))
      cuenta))

#|
Des: Permite añadir el nombre a la lista
Dom: lista de followers, el nombre a agregar
Rec: La lista de followers modificada
|#
(define (addListFollow_encaps listCuenta nombre)
  (if (not(null? listCuenta))
      (cons (car listCuenta) (addListFollow_encaps (cdr listCuenta) nombre))
      (cons nombre null)))

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
      (cons publicacion null)))

;####################################################################

#|
Des: Permite añadir una publicacion a la lista de publicaciones compartidas de una cuenta
Dom: La cuenta que se le agregara y la publicacion a agregar
Rec: Una nueva cuenta modificada
|#
(define (addPublicacionCompartidas cuenta publicacion name)
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
              (addPublicacionCompartidas_encaps (getListPublicacionesCompartidas_C cuenta) publicacion name))
     cuenta))

#|
Des: Permite añadir una publicacion a la lista de publicaciones compartidas
Dom: La lista de publicaciones y la publicacion
Rec: La lista de publicaciones modificada
|#
(define (addPublicacionCompartidas_encaps PublicacionesCompartidas publicacion name)
  (if (not(null? PublicacionesCompartidas))
      (cons (car PublicacionesCompartidas) (addPublicacionCompartidas_encaps (cdr PublicacionesCompartidas) publicacion name))
      (cons (list publicacion name) null)))