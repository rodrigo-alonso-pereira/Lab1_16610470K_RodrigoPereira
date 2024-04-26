#lang racket

(provide (all-defined-out))

;; Funcion que entrega un 'car-type' de un pcar

; Dom: pcar (pcar)
; Rec: car-type

(define pcar-get-car-type
  (lambda (pcar)
    (last pcar)))
