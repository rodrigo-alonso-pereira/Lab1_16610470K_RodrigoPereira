#lang racket

(provide (all-defined-out))

;; Funcion que entrega un 'id' de una estacion

; Dom: station (station)
; Rec: id (int)

(define station-get-id
  (lambda (station)
    (first station)))

;; Funcion que entrega un 'name' de una estacion

; Dom: station (station)
; Rec: name (String)

(define station-get-name
  (lambda (station)
    (second station)))


;; Funcion que entrega un 'stop-time' de una estacion

; Dom: station (station)
; Rec: stop-time (positive integer)

(define station-get-stop-time
  (lambda (station)
    (last station)))