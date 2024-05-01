#lang racket

(provide (all-defined-out))

;; Funcion que entrega un 'point1' de una seccion

; Dom: section (section)
; Rec: point1 (station)

(define section-get-point1
  (lambda (section)
    (first section)))

;; Funcion que entrega un 'point2' de una seccion

; Dom: section (section)
; Rec: point2 (station)

(define section-get-point2
  (lambda (section)
    (second section)))


;; Funcion que entrega un 'distance' de una seccion

; Dom: section (section)
; Rec: distance (positive-number)

(define section-get-distance
  (lambda (section)
    (third section)))


;; Funcion que entrega un 'cost' de una seccion

; Dom: section (section)
; Rec: cost (positive-number U {0})

(define section-get-cost
  (lambda (section)
    (last section)))


