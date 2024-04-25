#lang racket

(provide (all-defined-out))

(require "TDA_Station.rkt")
(require "TDA_Line.rkt")
(require "TDA_Section.rkt")

; Definicion de station-type
(define r "Regular")
(define m "Mantencion")
(define c "Combinacion")
(define t "Terminal")

;; Req 2: TDA station - constructor

; Dom: id (int) X name (String)  X type (String) X stop-time (positive integer)
; Rec: station

(define station
  (lambda (id name type stop-time)
    (list id name type stop-time)))

; Estaciones L1 simplificada metro santiago
(define e0 (station 0 "San Pablo" t 90))
(define e1 (station 1 "Neptuno" r 45))
(define e2 (station 2 "Pajaritos" c 45))
(define e3 (station 3 "Las Rejas" r 45))
(define e4 (station 4 "Ecuador" r 60))
(define e5 (station 5 "San Alberto Hurtado" r 40))
(define e6 (station 6 "Universidad de Santiago de Chile" c 40))
(define e7 (station 7 "Estación Central" c 45))
(define e8 (station 8 "Unión Latinoamericana" r 30))
(define e9 (station 9 "República" r 40))
(define e10 (station 10 "Los Héroes" c 60))
(define e11 (station 11 "La Moneda" r 40))
(define e12 (station 12 "Universidad de Chile" c 90))
(define e13 (station 13 "Santa Lucía" r 40))
(define e14 (station 14 "Universidad Católica" c 60))
(define e15 (station 15 "Baquedano" r 40))
(define e16 (station 16 "Los Dominicos" t 90))
(define e17 (station 17 "Cochera Neptuno" m 3600))

; Estaciones L2 simplificada metro santiago, para una versión circular
(define e18 (station 18 "El Llano" r 60))
(define e19 (station 19 "Franklin" r 50))
(define e20 (station 20 "Rondizzoni" r 55))
(define e21 (station 21 "Parque O'Higgins" r 65))
(define e22 (station 22 "Toesca" r 65))
(define e23 (station 23 "Santa Ana" c 65))
(define e24 (station 24 "Puente Cal y Canto" r 65))


;; Req 3: TDA section - constructor

; Dom: point1 (station)  X point2 (station) X distance (positive-number) X cost (positive-number U {0}).
; Rec: section

(define section
  (lambda (point1 point2 distance cost)
    (list point1 point2 distance cost)))

;Tramos Línea 1
(define s0 (section e0 e1 4 15))
(define s1 (section e1 e2 3 14))
(define s2 (section e2 e3 2.5 10))
(define s3 (section e3 e4 4.5 17))
(define s4 (section e4 e5 4.7 18))
(define s5 (section e5 e6 4.3 17))
(define s6 (section e6 e7 3.8 12))
(define s7 (section e7 e8 2.5 10))
(define s8 (section e8 e9 4.5 17))
(define s9 (section e9 e10 4.7 18))
(define s10 (section e10 e11 4.3 17))
(define s11 (section  e11 e12 3.8 12))
(define s12 (section e12 e13 4.5 17))
(define s13 (section e13 e14 4.7 18))
(define s14 (section e14 e15 4.3 17))
(define s15 (section e15 e16 4.2 17))
;enlace cochera
(define s16 (section e1 e17 3.8 12))

; Tramos Línea 2, línea circular
(define s17 (section e18 e19 4 15))
(define s18 (section e19 e20 3 12))
(define s19 (section e20 e21 5 18))
(define s20 (section e21 e22 4.5 16))
(define s21 (section e22 e10 4.2 16))
(define s22 (section e10 e23 4.2 16))
(define s23 (section e23 e24 4.2 16))
(define s24 (section e24 e18 28 90))


;; Req 4: TDA line - constructor

; Dom = id (int) X name (string) X rail-type (string) X section* (* señala que se pueden agregar 0 o más tramos)
; Rec = line

(define line
  (lambda (id name rail-type . section)
    (list id name rail-type section)))

;Creación de Línea 1 con todos los tramos
(define l1 (line 1 "Línea 1" "UIC 60 ASCE" s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15))
;Creación de Línea 2 sin incluir tramos
(define l2 (line 2 "Línea 2" "100 R.E."))


;; Req 5: TDA line - otras funciones

; Dom = line (line)
; Rec = positive-number

(define line-lenght
  (lambda (line)
    (apply +(map (lambda (x) (section-get-distance x)) (line-get-section line)))))

(line-lenght l1) ;resultado debe ser 64,3 si considera inclusive los tramos hacia estaciones de mantenimiento 
(line-lenght l2) ;resultado debe ser 0


;; Req 6: TDA line - otras funciones

; Dom = line (line) X station1-name (String) X station2-name (String)
; Rec = positive-number
; Recursividad = Cola

(define line-section-length
  (lambda (line station1-name station2-name)
    (define line-section-lenght-int
      (lambda (lst flag name1 name2 acc)
        (cond
          [(null? lst) acc]
          [(eq? (station-get-name (first (car lst))) name1) (line-section-lenght-int (cdr lst) #t name1 name2 (+ (section-get-distance (car lst)) acc))]
          [(eq? (station-get-name (second (car lst))) name2) (if flag
                                                               (+ (section-get-distance (car lst)) acc)
                                                               (line-section-lenght-int (cdr lst) flag name1 name2 acc))]
          [else (if flag
                    (line-section-lenght-int (cdr lst) flag name1 name2 (+ (section-get-distance (car lst)) acc))
                    (line-section-lenght-int (cdr lst) flag name1 name2 acc))])))
    (line-section-lenght-int (line-get-section line) #f station1-name station2-name 0)))
          

(line-section-length l1 "San Pablo" "Las Rejas") ;respuesta es 9.5


;; Req 7: TDA line - otras funciones

; Dom = line (line)
; Rec = positive-number U {0}
; Recursividad = Natural

(define line-cost
  (lambda (line)
    (define line-cost-map-int
      (lambda (fn-map fn-apply lst)
        (cond
          [(null? lst) 0]
          [(null? (cdr lst)) (fn-map (car lst))]
          [else (fn-apply (fn-map (car lst)) (line-cost-map-int fn-map fn-apply (cdr lst)))])))
    (line-cost-map-int (lambda (x) (section-get-cost x)) + (line-get-section line))))

(line-cost l1) ;resultado debe ser 246 si considera inclusive los tramos hacia estaciones de mantenimiento 
(line-cost l2) ;resultado debe ser 0


;; Req 8: TDA line - otras funciones

; Dom = line (line) X station1-name (String) X station2-name (String)
; Rec = positive-number U {0}
; Recursividad = Cola

(define line-section-cost
  (lambda (line station1-name station2-name)
    (define line-section-cost-int
      (lambda (lst flag name1 name2 acc)
        (cond
          [(null? lst) acc]
          [(eq? (station-get-name (first (car lst))) name1) (line-section-cost-int (cdr lst) #t name1 name2 (+ (section-get-cost (car lst)) acc))]
          [(eq? (station-get-name (second (car lst))) name2) (if flag
                                                               (+ (section-get-cost (car lst)) acc)
                                                               (line-section-cost-int (cdr lst) flag name1 name2 acc))]
          [else (if flag
                    (line-section-cost-int (cdr lst) flag name1 name2 (+ (section-get-cost (car lst)) acc))
                    (line-section-cost-int (cdr lst) flag name1 name2 acc))])))
    (line-section-cost-int (line-get-section line) #f station1-name station2-name 0)))

(line-section-cost l1 "San Pablo" "Las Rejas") ;respuesta es 39


;; Req 9: TDA line - modificador

; Dom = line (line) X section (section)
; Rec = line
; Recursividad = Natural

(define verify-section-line
  (lambda (lst-section section)
    (cond
      [(null? lst-section) #f]
      [else (if (eq? section (car lst-section))
                #t
                (verify-section-line (cdr lst-section) section))])))

(define line-add-section
  (lambda (line section)
    (if (verify-section-line (line-get-section line) section)
        line
        (list (line-get-id line)
              (line-get-name line)
              (line-get-rail-type line)
              (reverse (cons section (reverse (line-get-section line))))))))
    
;añadiendo tramos a l2
(define l2a (line-add-section l2 s17))
(define l2b (line-add-section l2a s18))
(define l2c (line-add-section l2b s19))
(define l2d (line-add-section l2c s20))
(define l2e (line-add-section l2d s21))
(define l2f (line-add-section l2e s22))
(define l2g (line-add-section l2f s23))
(define l2h (line-add-section l2g s24))
(define l2i (line-add-section l2h s19))
#|
Dependiendo de como implemente la función, esta operación no añade la estación duplicada.
Puede lanzar un “error o excepción” (no un mensaje de error como String, para no
comprometer el recorrido de la función) o bien devolver la línea de entrada intacta.
En este caso, l2i sería igual a l2h. 
|#


;; Req9: TDA Línea - pertenencia

; Dom = line (line)
; Rec = boolean
; Recursividad = elegir alguna.

#|
(define line?
  (lambda (line)
    (
|#

#|
(line? l1)  ;devuelve true
(line? l2)  ;devuelve false
(line? l2e)  ;devuelve false
(line? l2h)  ;devuelve true
|#


; Definicion de car-type
(define tr "Terminal")
(define ct "Central")

;; Req10: TDA pcar - Constructor

; Dom = id (int) X capacity (positive integer) X model (string) X type (car-type)
; Rec = pcar

(define pcar
  (lambda (id capacity model type)
    (list id capacity model type)))

;creando carros
(define pc0 (pcar 0 100 "NS-74" tr))
(define pc1 (pcar 1 100 "NS-74" ct))
(define pc2 (pcar 2 150 "NS-74" ct))
(define pc3 (pcar 3 100 "NS-74" ct))
(define pc4 (pcar 4 100 "NS-74" tr))
(define pc5 (pcar 5 100 "AS-2014" tr))
(define pc6 (pcar 6 100 "AS-2014" ct))
(define pc7 (pcar 7 100 "AS-2014" ct))
(define pc8 (pcar 8 100 "AS-2014" ct))
(define pc9 (pcar 9 100 "AS-2014" tr))
(define pc10 (pcar 10 100 "AS-2014" tr))
(define pc11a (pcar 11 100 "AS-2016" tr))
(define pc11 (pcar 12 100 "AS-2016" ct))
(define pc12 (pcar 13 100 "AS-2016" ct))
(define pc13 (pcar 14 150 "AS-2016" ct))
(define pc14 (pcar 15 100 "AS-2016" ct))
(define pc15 (pcar 16 100 "AS-2016" ct))
(define pc16 (pcar 17 100 "AS-2016" ct))
(define pc17 (pcar 18 100 "AS-2016" tr))


;; Req11: TDA train - Constructor

; Dom = id (int) X maker (string) X rail-type (string) X speed (positive number) X station-stay-time (positive number U {0}) X pcar* (* indica que pueden especificarse 0 o más carros)
; Rec = train

(define verify-section-line
  (lambda (lst-section section)
    (cond
      [(null? lst-section) #f]
      [else (if (eq? section (car lst-section))
                #t
                (verify-section-line (cdr lst-section) section))])))

(define verify-train-car-type
  (lambda (lst)
    (cond
      [(null? lst) #f]
      [else (if (; verificar q primero y ultimo sea tr y el resto ct

(define train
  (lamda (id maker rail-type speed station-stay-time . pcar)
         (if (verify-train-car-type pcar)
             (list id maker rail-type speed station-stay-time pcar)
             null)))

;creando trenes
(define t0 (train 0 "CAF" "UIC 60 ASCE" 60 1.5)) ;tren sin carros definidos
(define t1 (train 1 "CAF" "UIC 60 ASCE" 70  2 pc0 pc1 pc2 pc3 pc4)) ;tren válido
(define t2 (train 2 "CAF" "100 R.E." 70  2 pc5 pc6 pc7 pc8 pc9)) ;tren válido
(define t3 (train 3 "CAF" "100 R.E." 70  2 pc11a pc11 pc12 pc13 pc14 pc15 pc16 pc17)) ;tren válido
(define t4 (train 4 "CAF" "100 R.E." 70  2 pc1 pc2 pc3)) ;tren inválido sin terminales en extremos
(define t5 (train 5 "CAF" "100 R.E." 70  2 pc0 pc5 pc9 pc12 pc17))  ;tren inválido por incompatibilidad de carros






