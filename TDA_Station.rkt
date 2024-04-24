#lang racket

(provide (all-defined-out))

(define station-get-name
  (lambda (station)
    (second station)))

(define station-get-id
  (lambda (station)
    (first station)))