#lang racket

(provide (all-defined-out))

;(require "main_16610470_PereiraYañez.rkt")

; Funcion que construye una linea
(define line
  (lambda (id name rail-type . section)
    (list id name rail-type section)))

; Funcion que entrega un 'id' de una linea
(define line-get-id
  (lambda (line)
    (first line)))

; Funcion que entrega un 'name' de una linea
(define line-get-name
  (lambda (line)
    (second line)))

; Funcion que entrega un 'rail-type' de una linea
(define line-get-rail-type
  (lambda (line)
    (third line)))

; Funcion que entrega una 'section' de una linea
(define line-get-section
  (lambda (line)
    (last line)))
