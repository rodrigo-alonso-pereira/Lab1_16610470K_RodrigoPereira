#lang racket

(provide (all-defined-out))

;; Funcion que entrega un 'id' de un tren

; Dom: train (train)
; Rec: id (int)

(define train-get-id
  (lambda (train)
    (first train)))


;; Funcion que entrega un 'maker' de un tren

; Dom: train (train)
; Rec: maker (string)

(define train-get-maker
  (lambda (train)
    (second train)))


;; Funcion que entrega un 'rail-type' de un tren

; Dom: train (train)
; Rec: rail-type (string)

(define train-get-rail-type
  (lambda (train)
    (third train)))


;; Funcion que entrega un 'speed' de un tren

; Dom: train (train)
; Rec: speed (positive number)

(define train-get-speed
  (lambda (train)
    (fourth train)))


;; Funcion que entrega un 'station-stay-time' de un tren

; Dom: train (train)
; Rec: station-stay-time (positive number U {0})

(define train-get-station-stay-time
  (lambda (train)
    (fifth train)))


;; Funcion que entrega una lista de 'pcar' de un tren

; Dom: train (train)
; Rec: pcar (list)

(define train-get-pcar
  (lambda (train)
    (last train)))
