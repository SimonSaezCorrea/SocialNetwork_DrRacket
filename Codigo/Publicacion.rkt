#lang racket
#|
Publicacion -> Autor | fecha publicacion | Tipo de publicacion | Contenido de la publicacion |
               Cantidad compartir
               cuenta | Date | String | String | Integer
|#


;Constructor

(define (publicacion autor fecha tipo contenido compartir)
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

;Pertenencia

(define (publicacion? publicacion)
  (if (string? (getAutor_P publicacion))
      (if (date? (getFecha_P publicacion))
          (if (string? (getTipo_P publicacion))
              (if(or (eqv? (getTipo_P publicacion) "photo")
                    (eqv? (getTipo_P publicacion) "video")
                    (eqv? (getTipo_P publicacion) "url")
                    (eqv? (getTipo_P publicacion) "text")
                    (eqv? (getTipo_P publicacion) "audio"))
                 (if (string? (getContenido_P publicacion))
                     (if (Integer? (getCompartir_P publicacion))
                         #t
                         #f)
                     #f)
                 #f)
              #f)
          #f)
      #f))

; Modificador

(define (setCompartir publicacion)
  (if (publicacion? publicacion)
      (list (getAutor_P publicacion) (getFecha_P publicacion) (getTipo_P publicacion)
            (getContenido_P publicacion) (getCantidad_P publicacion))
      null))