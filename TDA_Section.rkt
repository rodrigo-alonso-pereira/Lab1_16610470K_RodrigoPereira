#lang racket

(provide (all-defined-out))

(define section-get-distance
  (lambda (section)
    (third section)))

(define section-get-cost
  (lambda (section)
    (last section)))

(define section-get-point1
  (lambda (section)
    (first section)))

(define section-get-point2
  (lambda (section)
    (second section)))


