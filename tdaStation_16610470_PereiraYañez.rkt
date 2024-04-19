#lang racket

;; TDA station - constructor

; Dom: id (int) X name (String)  X type (String) X stop-time (positive integer)
; Rec: station

; TODO:
; Modificar: type (String) -> type (station-type)

(define station
  (lambda (id name type stop-time)
    (list id name type stop-time)))

;creando nueva estacion
(define e0 (station 1 "USACH" "c" 30))
e0
(define e1 (station 2 "Estación Central" "c" 45))
e1
(define e2 (station 3 "ULA" "r" 45))
e2
(define e3 (station 3 "República" "r" 45))
e3
(define e4 (station 4 "Los Héroes" "c" 60))
e4
(define e5 (station 5 "Toesca" "r" 40))
e5
(define e6 (station 6 "La Moneda" "r" 40))
e6
(define e7 (station 7 "Cochera" "m" 3600))
e7
(define e8 (station 8 "Parque OHiggins" "r" 30))
e8
(define e9 (station 9 "San Pablo" "t" 40))
e9
(define e10 (station 10 "Los Dominicos" "t" 60))
e10


;; TDA section - constructor

; Dom: point1 (station)  X point2 (station) X distance (positive-number) X cost (positive-number U {0}).
; Rec: section

(define section
  (lambda (point1 point2 distance cost)
    (list point1 point2 distance cost)))

; creando nueva estacion
(define s0 (section e0 e1 2 50))
s0
(define s1 (section e1 e2 2.5 55))
s1
(define s2 (section e2 e3 1.5  30))
s2
(define s3 (section e3 e4 3  45))
s3
(define s4 (section e4 e5 3  45))
s4
(define s5 (section e4 e6 1.4  50))
s5
(define s6 (section e5 e8 2  40))
s6
(define s7 (section e0 e7 3  0))
s7
(define s8 (section e0 e9 7  100))
s8
(define s9 (section e6 e10 15  250))
s9


;; TDA line - constructor

; Dom = id (int) X name (string) X rail-type (string) X section* (* señala que se pueden agregar 0 o más tramos)
; Rec = line

(define line
  (lambda (id name rail-type . section)
    (list id name rail-type section)))

; creando nuevas lineas
(define l0 (line 0 "Línea 0" "UIC 60 ASCE"))
l0
(define l1 (line 1 "Línea 1" "100 R.E." s0 s1 s2 s3 s5 s7 s8 s9))
l1


;; TDA line - otras funciones

; Dom = line (line)
; Rec = positive-number

(define line-get-section
  (lambda (line)
    (last line)))

(define section-get-distance
  (lambda (section)
    (third section)))

(define line-lenght
  (lambda (line)
    (apply +(map (lambda (x) (section-get-distance x)) (line-get-section line)))))

; Calculando distancia l1
(line-lenght l1)


;; TDA line - otras funciones

; Dom = line (line) X station1-name (String) X station2-name (String)
; Rec = positive-number

; Obtener de seccion con station1-name, distancia 1
; Obtener de seccion con station2-name, distancia 2
; Calcular diferencia y entregar numero

;(define line-section-length
;  (lambda (line station1-name station2-name)
;    (

; Calculando distancia entre "USACH" y "Los Heroes"
; (line-section-length l1 “USACH” “Los Héroes”)


;; TDA line - otras funciones

; Dom = line (line)
; Rec = positive-number U {0}

(define section-get-cost
  (lambda (section)
    (last section)))

; Recursividad natural
(define line-cost
  (lambda (line)
    (define line-cost-map-int
      (lambda (fn-map fn-apply lst)
        (cond
          [(null? lst) 0]
          [(null? (cdr lst)) (fn-map (car lst))]
          [else (fn-apply (fn-map (car lst)) (line-cost-map-int fn-map fn-apply (cdr lst)))])))
    (line-cost-map-int (lambda (x) (section-get-cost x)) + (line-get-section line))))

(line-cost l1)


;; TDA line - otras funciones

; Dom = line (line) X station1-name (String) X station2-name (String)
; Rec = positive-number U {0}

; Obtener seccion con name1
; Obtener seccion con name2 -> Filter
; Obtiene costo 1 + costo 2


; Recursividad de Cola
(define line-section-cost
  (lambda (line station1-name station2-name)
    (define line-section-cost-int
      (lambda (line station1-name station2-name lst acc)
        (cond
          [(null? lst) acc]
          [else (line-section-cost-int 


