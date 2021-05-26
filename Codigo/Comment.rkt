#lang racket

(require "date.rkt")

(provide (all-defined-out))

(define (comentario ID autor comment date likes)
  (list ID autor comment date likes))

(define (getID_Comment comentario)
  (car comentario))

(define (getAutor_Comment comentario)
  (car (cdr comentario)))

(define (getComment_Comment comentario)
  (car (cdr (cdr comentario))))

(define (getDate_Comment comentario)
  (car (cdr (cdr (cdr comentario)))))

(define (getLikes_Comment comentario)
  (car (cdr (cdr (cdr (cdr comentario))))))

(define (comentario? comentario)
  (and (integer? (getID_Comment comentario))
       (string? (getAutor_Comment comentario))
       (string? (getComment_Comment comentario))
       (day? (getDate_Comment comentario))
       (integer? (getLikes_Comment comentario))))