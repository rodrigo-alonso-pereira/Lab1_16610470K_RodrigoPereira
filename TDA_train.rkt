#lang racket

(provide (all-defined-out))

;; Funcion que entrega una lista de 'pcar' de un tren

; Dom: train (train)
; Rec: pcar (list)

(define train-get-pcar
  (lambda (train)
    (last train)))
