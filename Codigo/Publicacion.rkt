#lang racket

(require "date.rkt")
(provide (all-defined-out))


#|
Publicacion -> Autor | fecha publicacion | Tipo de publicacion | Contenido de la publicacion | ID | Comentarios | likes publicacion | cant compartidas
               String | Date | String | String | Integer | list de list string y like | integer | integer
|#


;Constructor

#|
Des: Permite la creacion de un constructor de tipo posting
Dom: autor (String), fecha (day), tipo (String), contenido (String), compartir (Integer), ID (Integer), comentarios (list list strings | integer), likesP (integer)
Rec: Una lista con los datos
|#
(define (posting autor fecha tipo contenido ID comentarios likesP cantComp)
  (if (and (string? autor)
           (day? fecha)
           (string? contenido)
           (integer? ID)
           (integer? likesP)
           (integer? cantComp))
      (list autor fecha tipo contenido ID comentarios likesP cantComp)
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
Des: Permite obtener el dato de ID
Dom: El dato posting(Una lista)
Rec: El dato ID
|#
(define (getID_P publicacion)
  (car (cdr (cdr (cdr (cdr publicacion))))))

#|
Des: Permite obtener el dato de comentarios
Dom: El dato posting(Una lista)
Rec: El dato comentario
|#
(define (getComentario_P publicacion)
  (car (cdr (cdr (cdr (cdr (cdr publicacion)))))))

#|
Des: Permite obtener el dato de likes publicacion
Dom: El dato posting(Una lista)
Rec: El dato likes publicacion
|#
(define (getLikesP_P publicacion)
  (car (cdr (cdr (cdr (cdr (cdr (cdr publicacion))))))))

#|
Des: Permite obtener el dato de cantCompartidas
Dom: El dato posting(Una lista)
Rec: El dato likes publicacion
|#
(define (getCantComp_P publicacion)
  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr publicacion)))))))))
  
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
       (integer? (getID_P publicacion))
       (integer? (getLikesP_P publicacion))
       (integer? (getCantComp_P publicacion))))

#|
Des: Permite crear un string con el contenido de un post
Dom: Publicacion, un string y la funcion de descriptacion
Rec: String
|#
(define (string->post Post descrypt)
  (string-append "    El ID del post es: " (number->string (getID_P Post)) "\n"
                 "    El autor de la publicacion es: " (getAutor_P Post) "\n"
                 "    la fecha de creacion fue: " (string->fecha (getFecha_P Post))
                 "    Es de tipo " (getTipo_P Post) "\n"
                 "    El contenido es: \n      " (descrypt (getContenido_P Post)) "\n"
                 "    Los comentarios son: \n" (string->Comentario (getComentario_P Post) descrypt "" 1)
                 "    Likes publicacion: " (number->string (getLikesP_P Post)) "\n"
                 "    Cantidad de veces compartidas fue de " (number->string (getCantComp_P Post)) " veces\n"))

(define (string->Comentario listComentario descrypt string i)
  (if(not(null? listComentario))
     (string->Comentario (cdr listComentario) descrypt (string-append string  (number->string i) ") " (descrypt (car (car listComentario))) "\n"
                                                                      "       likes: " (number->string(car (cdr (car listComentario)))) "\n"
                                                                      "       " (string->fecha (car(cdr(cdr (car listComentario)))))) (+ i 1))
     string))



(define (addComentario post date comment)
  (list (getAutor_P post)
        (getFecha_P post)
        (getTipo_P post)
        (getContenido_P post)
        (getID_P post)
        (addComentario_encaps (getComentario_P post) date comment)
        (getLikesP_P post)
        (+ (getCantComp_P post) 1)))

(define (addComentario_encaps  listComent date commet)
  (if (not(null? listComent))
      (cons (car listComent) (addComentario_encaps (cdr listComent) date commet))
      (cons commet null)))