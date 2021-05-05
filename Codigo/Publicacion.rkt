#lang racket

(require "date.rkt")

(provide post)
(provide post?)
(provide setCompartir)
(provide )

#|
Publicacion -> Autor | fecha publicacion | Tipo de publicacion | Contenido de la publicacion |
               Cantidad compartir | ID
               cuenta | Date | String | String | Integer | Integer
|#


;Constructor

(define (post autor fecha tipo contenido compartir)
  (list autor fecha tipo contenido compartir))


;Selector

(define (getAutor_P publicacion)
  (car publicacion))

(define (getFecha_P publicacion)
  (car (cdr publicacion)))

(define (getTipo_P publicacion)
  (car (cdr (cdr publicacion))))

(define (getContenido_P publicacion)
  (car (cdr (cdr publicacion))))

(define (getCompartir_P publicacion)
  (car (cdr (cdr (cdr publicacion)))))

(define (getID_P publicacion)
  (car (cdr (cdr (cdr (cdr publicacion))))))

;Pertenencia

(define (post? publicacion)
  (if (string? (getAutor_P publicacion))
      (if (date? (getFecha_P publicacion))
          (if (string? (getTipo_P publicacion))
              (if(or (eqv? (getTipo_P publicacion) "photo")
                    (eqv? (getTipo_P publicacion) "video")
                    (eqv? (getTipo_P publicacion) "url")
                    (eqv? (getTipo_P publicacion) "text")
                    (eqv? (getTipo_P publicacion) "audio"))
                 (if (string? (getContenido_P publicacion))
                     (if (integer? (getCompartir_P publicacion))
                         (if (integer? (getID_P publicacion))
                             #t
                             #f)
                         #f)
                     #f)
                 #f)
              #f)
          #f)
      #f))

; Modificador

(define (setCompartir publicacion cantidad)
  (if (post? publicacion)
      (post (getAutor_P publicacion) (getFecha_P publicacion) (getTipo_P publicacion)
            (getContenido_P publicacion) cantidad (getID_P publicacion))
      null))