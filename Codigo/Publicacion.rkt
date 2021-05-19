#lang racket

(require "date.rkt")


(provide getAutor_P)
(provide getFecha_P)
(provide getTipo_P)
(provide getContenido_P)
(provide getCompartir_P)
(provide getID_P)

(provide posting)
(provide post?)
(provide setCompartir)

#|
Publicacion -> Autor | fecha publicacion | Tipo de publicacion | Contenido de la publicacion |
               Cantidad compartir | ID
               String | Date | String | String | Integer | Integer
|#


;Constructor

#|
Des: Permite la creacion de un constructor de tipo posting
Dom: autor (String), fecha (day), tipo (String), contenido (String), compartir (Integer), ID (Integer)
Rec: Una lista con los datos
|#
(define (posting autor fecha tipo contenido compartir ID)
  (if (and (string? autor)
           (day? fecha)
           (string? contenido)
           (integer? compartir)
           (integer? ID))
      (list autor fecha tipo contenido compartir ID)
      null))


;Selector

#|
Des: Permite obtener el dato de autor
Dom: El dato posting(Una lista)
Rec: El dato autor
|#
(define (getAutor_P publicacion)
  (car publicacion))

#|
Des: Permite obtener el dato de fecha
Dom: El dato posting(Una lista)
Rec: El dato fecha
|#
(define (getFecha_P publicacion)
  (car (cdr publicacion)))

#|
Des: Permite obtener el dato de tipo
Dom: El dato posting(Una lista)
Rec: El dato tipo
|#
(define (getTipo_P publicacion)
  (car (cdr (cdr publicacion))))

#|
Des: Permite obtener el dato de contenido
Dom: El dato posting(Una lista)
Rec: El dato contenido
|#
(define (getContenido_P publicacion)
  (car (cdr (cdr (cdr publicacion)))))

#|
Des: Permite obtener el dato de compartir
Dom: El dato posting(Una lista)
Rec: El dato compartir
|#
(define (getCompartir_P publicacion)
  (car (cdr (cdr (cdr (cdr publicacion))))))

#|
Des: Permite obtener el dato de ID
Dom: El dato posting(Una lista)
Rec: El dato ID
|#
(define (getID_P publicacion)
  (car (cdr (cdr (cdr (cdr (cdr publicacion)))))))

;Pertenencia

#|
Des: Permite saber si es un dato tipo publicacion
Dom: El dato posting(Una lista)
Rec: Sentencia booleana
|#
(define (post? publicacion)
  (and (string? (getAutor_P publicacion))
       (day? (getFecha_P publicacion))
       (string?(getTipo_P publicacion))
       (or (equal? (getTipo_P publicacion) "photo")
           (equal? (getTipo_P publicacion) "video")
           (equal? (getTipo_P publicacion) "url")
           (equal? (getTipo_P publicacion) "text")
           (equal? (getTipo_P publicacion) "audio"))
       (string? (getContenido_P publicacion))
       (integer? (getCompartir_P publicacion))
       (integer? (getID_P publicacion))))

; Modificador

#|
Des: Permite cambiar el dato de compartir
Dom: El dato posting( Una lista) y el nuevo numero (Integer)
Rec: El posting modificado
|#
(define (setCompartir publicacion cantidad)
  (if (post? publicacion)
      (posting (getAutor_P publicacion) (getFecha_P publicacion) (getTipo_P publicacion)
            (getContenido_P publicacion) cantidad (getID_P publicacion))
      publicacion))