#lang racket

(provide (all-defined-out))

;; Funcion que entrega un 'id' de un subway

; Dom: subway (subway)
; Rec: id (int)

(define subway-get-id
  (lambda (subway)
    (first subway)))


;; Funcion que entrega un 'nombre' de un subway

; Dom: subway (subway)
; Rec: nombre (string)

(define subway-get-id
  (lambda (subway)
    (second subway)))