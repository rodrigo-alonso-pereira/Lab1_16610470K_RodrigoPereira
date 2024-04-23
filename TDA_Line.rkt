#lang racket

(provide (all-defined-out))

; Funcion que entrega una seccion de una linea
(define line-get-section
  (lambda (line)
    (last line)))