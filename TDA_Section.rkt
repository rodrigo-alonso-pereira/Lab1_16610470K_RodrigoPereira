#lang racket

(provide (all-defined-out))

(define section-get-distance
  (lambda (section)
    (third section)))

(define section-get-cost
  (lambda (section)
    (last section)))

(define section-get-id
  (lambda (section)
    (last section)))