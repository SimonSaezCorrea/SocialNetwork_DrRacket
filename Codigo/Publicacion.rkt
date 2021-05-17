#lang racket

(require "date.rkt")
;(require "Cuenta.rkt")

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

(define (posting autor fecha tipo contenido compartir ID)
  (if (and (string? autor)
           (day? fecha)
           (string? contenido)
           (integer? compartir)
           (integer? ID))
      (list autor fecha tipo contenido compartir ID)
      null))


;Selector

(define (getAutor_P publicacion)
  (car publicacion))

(define (getFecha_P publicacion)
  (car (cdr publicacion)))

(define (getTipo_P publicacion)
  (car (cdr (cdr publicacion))))

(define (getContenido_P publicacion)
  (car (cdr (cdr (cdr publicacion)))))

(define (getCompartir_P publicacion)
  (car (cdr (cdr (cdr (cdr publicacion))))))

(define (getID_P publicacion)
  (car (cdr (cdr (cdr (cdr (cdr publicacion)))))))

;Pertenencia

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

(define (setCompartir publicacion cantidad)
  (if (post? publicacion)
      (posting (getAutor_P publicacion) (getFecha_P publicacion) (getTipo_P publicacion)
            (getContenido_P publicacion) cantidad (getID_P publicacion))
      publicacion))