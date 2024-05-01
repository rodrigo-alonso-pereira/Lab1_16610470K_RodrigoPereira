#lang racket

(provide (all-defined-out))

;; Funcion que entrega un 'id' de una linea

; Dom: line (line)
; Rec: id (positive-number)

(define line-get-id
  (lambda (line)
    (first line)))


;; Funcion que entrega un 'name' de una linea

; Dom: line (line)
; Rec: name (string)

(define line-get-name
  (lambda (line)
    (second line)))


;; Funcion que entrega un 'rail-type' de una linea

; Dom: line (line)
; Rec: rail-type (string)

(define line-get-rail-type
  (lambda (line)
    (third line)))


;; Funcion que entrega una 'section' de una linea

; Dom: line (line)
; Rec: section (section)

(define line-get-section
  (lambda (line)
    (last line)))
