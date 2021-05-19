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

(provide existeUsuario?)
(provide addCuenta_SN)
(provide contadorCuentas)
(provide activar)
(provide buscarCuentaActiva)
(provide desactivar)

;Constructor

#|
Des: Permite la creacion de un constructor de tipo account
Dom: name (string), date (day), encryptFunction (Funcion de encriptacion), decryptFunction (Funcion de descriptacion)
Rec: Una lista con los datos y se le agrega una lista vacia para los usuarios
|#
(define (socialnetwork name date encryptFunction decryptFunction)
  (if (and (string? name)
           (day? date))
      (list name date encryptFunction decryptFunction (list))
      null))

;Selectores

#|
Des: Permite recoger el nombre
Dom: El dato socialnetwork
Rec: El nombre
|#
(define (getName_SN SN)
  (car SN))

#|
Des: Permite recoger el day
Dom: El dato socialnetwork
Rec: El day
|#
(define (getDate_SN SN)
  (car (cdr SN)))

#|
Des: Permite recoger el encrypt
Dom: El dato socialnetwork
Rec: El encrypt
|#
(define (getEncryptFunction_SN SN)
  (car (cdr (cdr SN))))

#|
Des: Permite recoger el decrypt
Dom: El dato socialnetwork
Rec: El decrypt
|#
(define (getDecryptFunction_SN SN)
  (car (cdr (cdr (cdr SN)))))

#|
Des: Permite recoger el cuenta
Dom: El dato socialnetwork
Rec: El cuenta
|#
(define (getCuenta_SN SN)
  (car (cdr (cdr (cdr (cdr SN))))))

;Pertenencia

#|
Des: Permite saber si la lista de cuentas tiene cuentas
Dom: La lista de cuentas
Rec: Sentencia booleana
|#
(define (Cuentas? Cuentas_SN)
  (if (not(null? Cuentas_SN))
      (if (account? (car Cuentas_SN))
          (Cuentas? (cdr Cuentas_SN))
          #f)
      #t))

#|
Des: Permite saber si es una socialnetwork
Dom: La socialnetwork (lista)
Rec: Sentencia booleana
|#
(define (socialnetwork? SN)
  (if (and (string? (getName_SN SN))
           (day? (getDate_SN SN))
           (Cuentas? (getCuenta_SN SN)))
      #t
      #f))

;Modificador

#|
Des: Permite añadir una cuenta a la socialnetwork
Dom: Una socialnetwork y la cuenta a añadir
Rec: la socialnetwork modificada
|#
(define (addCuenta_SN SN cuenta)
  (if(and(socialnetwork? SN)
         (account? cuenta))
     (list (getName_SN SN)
                    (getDate_SN SN)
                    (getEncryptFunction_SN SN)
                    (getDecryptFunction_SN SN)
                    (addCuenta (getCuenta_SN SN) cuenta))
     SN))

#|
Des: Permite añadir la cuenta a la lista de cuentas
Dom: La lista de cuentas y la cuenta
Rec: La lista modificada
|#
(define (addCuenta Cuentas cuenta)
  (if (not(null? Cuentas))
      (cons (car Cuentas) (addCuenta (cdr Cuentas) cuenta))
      (cons cuenta null)))




;Otras funciones

#|
Des: Permite sacar el ID de la cuenta
Dom: La lista de cuentas y el contador
Rec: El contador
|#
(define (contadorCuentas Cuentas contador)
  (if (and (Cuentas? Cuentas)
           (integer? contador))
      (if (not (null? Cuentas))
          (contadorCuentas (cdr Cuentas) (+ contador 1))
          contador)
      null))


#|
Des: Permite comprobar si existe el usuario en el socialnetwork
Dom: socialnetowork, nombre (String)
Rec: Sentencia booleana
|#
(define (existeUsuario? SN nombre)
  (if (and (string? nombre)
           (socialnetwork? SN))
      (existeUsuario?_encaps (getCuenta_SN SN) nombre)
      #f))

#|
Des: Permite comprobar si existe el usuario en la lista de usuarios
Dom: lista de usuarios, nombre (String)
Rec: Sentencia booleana
|#
(define (existeUsuario?_encaps listUsuarios nombre)
  (if (not(null? listUsuarios))
      (if (eqv? nombre (getNombre_C (car listUsuarios)))
          #f
          (existeUsuario?_encaps (cdr listUsuarios) nombre))
      #t))
       
;####################################################################################

#|
Des: Permite activar una cuenta
Dom: el socialnetwork, una cuenta y su contraseña
Rec: La socialnetwork con la cuenta activada
|#
(define (activar SN cuenta password)
  (if (and (string? cuenta)
           (string? password)
           (socialnetwork? SN))
      (if (not (null? (buscarCuenta_activar (getCuenta_SN SN) cuenta password)))
          (list (getName_SN SN) (getDate_SN SN) (getEncryptFunction_SN SN) (getDecryptFunction_SN SN)
                (buscarCuenta_activar (getCuenta_SN SN) cuenta password))
          SN)
      SN))

#|
Des: Permite activar la cuenta
Dom: La lista de cuentas, una cuenta y su contraseña
Rec: La lista de cuentas con la cuenta acticada
|#
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

;#######################################################################################################

#|
Des: Permite descativar la cuenta
Dom: La socialnetwork
Rec: La socialnetwork con la cuenta desactivada
|#
(define (desactivar SN)
  (if (and (socialnetwork? SN))
      (if (not (null? (buscarCuenta_desactivar (getCuenta_SN SN))))
          (list (getName_SN SN) (getDate_SN SN) (getEncryptFunction_SN SN) (getDecryptFunction_SN SN)
                (buscarCuenta_desactivar (getCuenta_SN SN)))
          SN)
      SN))

#|
Des: Permite desactivar la cuenta
Dom: La lista de cuentas
Rec: La lista de cuentas con la cuenta desactivada
|#
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

;######################################################################################

#|
Des: Permite buscar una cuenta activa
Dom: La socialNetwork
Rec: La cuenta activada
|#
(define (buscarCuentaActiva SN)
  (if (socialnetwork? SN)
      (buscarCuentaActiva_encaps (getCuenta_SN SN))
      null))

#|
Des: Permite buscar una cuenta activa
Dom: La lista de cuentas
Rec: La cuenta activada
|#
(define (buscarCuentaActiva_encaps listCuenta)
  (if (not (null? listCuenta))
      (if (eqv? (getActividad_C (car listCuenta)) #t)
          (car listCuenta)
          (buscarCuentaActiva_encaps (cdr listCuenta)))
      null))

;#########################################################################################
#|
#|
Des: Permite buscar una cuenta por usuario en una socialnetwork
Dom: socialnetwork y un usuario (string)
Rec: retorna una cuenta
|#
(define (buscarCuenta SN usuario)
  (if (and (socialnetwork? SN)
           (string? usuario))
      ((buscarCuenta_encaps (getCuenta_SN SN) usuario))))

#|
Des: Permite buscar una cuenta por un usuario en una lista de usuario
Dom: lista de usuario y un usuario (string)
Rec: retorna una cuenta
|#
(define (buscarCuenta_encaps listaUsuario usuario)
  (if (not (null? listaUsuario))
      (if (eqv? (getNombre_C (car listaUsuario)) usuario)
          (car listaUsuario)
          (buscarCuenta_encaps (cdr listaUsuario) usuario))
      null))
|#