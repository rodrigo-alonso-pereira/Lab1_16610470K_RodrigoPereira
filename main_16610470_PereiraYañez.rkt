#lang racket

(require "TDA_station.rkt")
(require "TDA_line.rkt")
(require "TDA_section.rkt")
(require "TDA_pcar.rkt")
(require "TDA_train.rkt")
(require "TDA_subway.rkt")

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

;Creando estaciones L1 simplificada metro santiago
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

;Creando estaciones L2 simplificada metro santiago, para una versión circular
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

;Creando tramos Línea 1
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

;Creando tramos Línea 2, línea circular
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

;creación de Línea 1 con todos los tramos
(define l1 (line 1 "Línea 1" "UIC 60 ASCE" s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15))
;creación de Línea 2 sin incluir tramos
(define l2 (line 2 "Línea 2" "100 R.E."))


;; Req 5: TDA line - otras funciones

; Dom = line (line)
; Rec = positive-number

(define line-lenght
  (lambda (line)
    (apply +(map (lambda (x) (section-get-distance x)) (line-get-section line)))))

;obteniendo distancia total de linea
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
    (if (and (eq? (station-get-name (first (car (line-get-section line)))) station1-name) (eq? (station-get-name (second (car (line-get-section line)))) station2-name))
        (section-get-distance (car (line-get-section line)))
        (line-section-lenght-int (line-get-section line) #f station1-name station2-name 0))))
          
;obteniendo distancia entre estaciones
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

;obteniendo costos de lineas
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

;obteniendo costos entre estaciones
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


;; Req10: TDA Línea - pertenencia

; Dom = line (line)
; Rec = boolean
; Recursividad = elegir alguna.

#|
(define line?
  (lambda (line)
    (
|#

#|
;validando lineas
(line? l1)  ;devuelve true
(line? l2)  ;devuelve false
(line? l2e)  ;devuelve false
(line? l2h)  ;devuelve true
|#


; Definicion de car-type
(define tr "Terminal")
(define ct "Central")

;; Req11: TDA pcar - Constructor

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


;; Req12: TDA train - Constructor

; Dom = id (int) X maker (string) X rail-type (string) X speed (positive number) X station-stay-time (positive number U {0}) X pcar* (* indica que pueden especificarse 0 o más carros)
; Rec = train
; Recursividad = Natural

(define verify-train-car-type
  (lambda (lst_pcar)
    (define verify-train-car-type-int
      (lambda (lst flag count)
        (cond
          [(null? lst) #t]
          [(eq? flag #t) (if (eq? (pcar-get-car-type (car lst)) tr)
                             (verify-train-car-type-int (cdr lst) #f (+ count 1))
                             #f)]
          [(null? (cdr lst)) (verify-train-car-type-int lst #t (+ count 1))]
          [else (if (eq? (pcar-get-car-type (car lst)) ct)
                    (verify-train-car-type-int (cdr lst) #f (+ count 1))
                    #f)])))
    (if (= 1 (length lst_pcar))
        #f
        (verify-train-car-type-int lst_pcar #t 0))))

(define train
  (lambda (id maker rail-type speed station-stay-time . pcar)
         (if (eq? (verify-train-car-type pcar) #t)
             (list id maker rail-type speed station-stay-time pcar)
             null)))

;creando trenes
(define t0 (train 0 "CAF" "UIC 60 ASCE" 60 1.5)) ;tren sin carros definidos
(define t1 (train 1 "CAF" "UIC 60 ASCE" 70  2 pc0 pc1 pc2 pc3 pc4)) ;tren válido
(define t2 (train 2 "CAF" "100 R.E." 70  2 pc5 pc6 pc7 pc8 pc9)) ;tren válido
(define t3 (train 3 "CAF" "100 R.E." 70  2 pc11a pc11 pc12 pc13 pc14 pc15 pc16 pc17)) ;tren válido
(define t4 (train 4 "CAF" "100 R.E." 70  2 pc1 pc2 pc3)) ;tren inválido sin terminales en extremos
(define t5 (train 5 "CAF" "100 R.E." 70  2 pc0 pc5 pc9 pc12 pc17))  ;tren inválido por incompatibilidad de carros
      

;; Req13: TDA train - Modificador

; Dom = train (train) X pcar (pcar) X position (positive-integer U {0})
; Rec = train
; Recursividad = Cola

(define train-add-car
  (lambda (train pcar position)
    (define train-add-car-in
      (lambda (lst pcar position count acc)
        (cond
          [(or (null? lst) (= count position)) (append (reverse acc) (list pcar) lst)]
          [else (train-add-car-in (cdr lst) pcar position (+ count 1) (cons (car lst) acc))])))
    (list (train-get-id train)
          (train-get-maker train)
          (train-get-rail-type train)
          (train-get-speed train)
          (train-get-station-stay-time train)
          (train-add-car-in (train-get-pcar train) pcar position 0 null))))

;agregando carros a convoy
(define t0a (train-add-car t0 pc5 0))
(define t0b (train-add-car t0a pc6 1))
(define t0c (train-add-car t0b pc7 2))
(define t0d (train-add-car t0c pc8 3))
(define t0e (train-add-car t0d pc9 4)) ;tren válido


;; Req14: TDA train - Modificador

; Dom = train (train) X position (positive-integer U {0})
; Rec = train
; Recursividad = Cola

(define train-remove-car
  (lambda (train position)
    (define train-remove-car-int
      (lambda (lst position count acc)
        (cond
          [(or (null? lst) (= count position)) (append (reverse acc) (cdr lst))]
          [else (train-remove-car-int (cdr lst) position (+ count 1) (cons (car lst) acc))])))
    (list (train-get-id train)
          (train-get-maker train)
          (train-get-rail-type train)
          (train-get-speed train)
          (train-get-station-stay-time train)
          (train-remove-car-int (train-get-pcar train) position 0 null))))

;removiendo carros a convoy
(define t1a (train-remove-car t1 0))
(define t1b (train-remove-car t1 2))


;; Req15: TDA train - Pertenencia

; Dom = train
; Rec = boolean
; Recursividad = Natural

(define train?
  (lambda (train)
    (define train?-int
      (lambda (lst model)
        (cond
          [(null? lst) #t]
          [else (if (eq? (pcar-get-model (car lst)) model)
                    (train?-int (cdr lst) model)
                    #f)])))
    (cond
      [(null? train) #f]
      [(null? (train-get-pcar train)) #f]
      [else (and (train?-int (train-get-pcar train) (pcar-get-model (car (train-get-pcar train)))) (verify-train-car-type (train-get-pcar train)))])))

;verificación de válidez en la conformación de trenes
(train? t0) ;debe arrojar #f
(train? t1) ;debe arrojar #t
(train? t2) ;debe arrojar #t
(train? t3) ;debe arrojar #t
(train? t4) ;debe arrojar #f
(train? t0a) ;debe arrojar #f
(train? t0b) ;debe arrojar #f
(train? t0c) ;debe arrojar #f
(train? t0d) ;debe arrojar #f
(train? t0e) ;debe arrojar #t
(train? t1a) ;debe arrojar #f
(train? t1b) ;debe arrojar #t


;; Req16: TDA train - Otras funciones

; Dom = train
; Rec = positive-number U {0}
; Recursividad = Natural

(define train-capacity
  (lambda (train)
    (define train-capacity-int
      (lambda (lst)
        (cond
          [(null? lst) 0]
          [else (+ (pcar-get-capacity (car lst)) (train-capacity-int (cdr lst)))])))
    (cond
      [(null? train) 0]
      [else (train-capacity-int (train-get-pcar train))])))
 
;determinar capacidad del tren
(train-capacity t0) ;debe arrojar 0
(train-capacity t1) ;debe arrojar 550


;; Req17: TDA driver - Constructor

; Dom = id (int) X nombre (string) X train-maker (string)
; Rec = driver

(define driver
  (lambda (id nombre train-maker)
    (list id nombre train-maker)))

;Creando drivers
(define d0 (driver 0 "Juan" "CAF"))
(define d1 (driver 1 "Alejandro" "Alsthom"))
(define d2 (driver 2 "Diego" "Alsthom"))
(define d3 (driver 3 "Pedro" "CAF"))


;; Req18: TDA subway - Constructor

; Dom = id (int) X nombre (string)
; Rec = subway

(define subway
  (lambda (id nombre)
    (list id nombre)))

;Creando subway
(define sw0 (subway 0 "Metro de Santiago"))
(define sw1 (subway 1 "Subte"))


;; Req19: TDA subway - Modificador

; Dom = sub (subway) X train+ (pueden ser 1 o más trenes)
; Rec = subway
; Recursividad = Cola

(define subway-add-train
  (lambda (sub . train)
    (define subway-add-train-int
      (lambda (lst acc)
        (cond
          [(null? lst) (reverse acc)]
          [else (subway-add-train-int (cdr lst) (cons (car lst) acc))])))
    (list (subway-get-id sub)
          (subway-get-nombre sub)
          (subway-add-train-int train null))))

;Agregando trenes
(define sw0a (subway-add-train sw0 t1 t2 t0e))


;; Req20: TDA subway - Modificador

; Dom = sub (subway) X line+ (pueden ser 1 o más líneas)
; Rec = subway

(define subway-add-line
  (lambda (sub . line)
    (list (subway-get-id sub)
          (subway-get-nombre sub)
          (third sub)
          line)))

;Agregando lineas
(define sw0b (subway-add-line sw0a l1 l2h))


;; Req21: TDA subway - Modificador

; Dom = sub (subway) X driver+ (pueden ser 1 o más conductores)
; Rec = subway

(define subway-add-driver
  (lambda (sub . driver)
    (list (subway-get-id sub)
          (subway-get-nombre sub)
          (third sub)
          (fourth sub)
          driver)))

;Agregando drivers
(define sw0c (subway-add-driver sw0b d0 d1 d2 d3))


;; Req22: TDA subway - Otras funciones

; Dom = sub (subway)
; Rec = String
; Rec = Natural / Declarativo

(define flatten-list
  (lambda (sub)
    (cond
      [(null? sub) null]
      [(not (list? (car sub))) (cons (car sub) (cons " " (flatten-list (cdr sub))))]
      [else (append (flatten-list (car sub)) (flatten-list (cdr sub)))])))

(define subway->string
  (lambda (sub)
    (apply string-append (map (lambda (x) (if (number? x) (number->string x) x)) (flatten-list sub)))))

;Expresado subway como string
(subway->string sw0c)


;; Req23: TDA subway - Modificador

; Dom = sub (subway) X function
; Rec = subway

(define subway-rise-section-cost
  (lambda (sub function)
    (define subway-rise-section-cost-int
      (lambda (lines fn)
        (map (lambda (line)
               (list (line-get-id line)
                     (line-get-name line)
                     (line-get-rail-type line)
                     (map (lambda (section)
                            (list (section-get-point1 section)
                                  (section-get-point2 section)
                                  (section-get-distance section)
                                  (fn (section-get-cost section))))
                      (line-get-section line))))
               lines)))
    (list (subway-get-id sub)
          (subway-get-nombre sub)
          (third sub)
          (subway-rise-section-cost-int (fourth sub) function)
          (fifth sub))))

;Aumentando los costos de las estaciones en un 30%
(define sw0d (subway-rise-section-cost sw0c (lambda (c) (* c 1.3))))


;; Req24: TDA subway - Modificador

; Dom = sub (subway) X stationName (String) X time
; Rec = subway

(define subway-set-station-stoptime
  (lambda (sub stationName time)
    (define subway-set-station-stoptime-int
      (lambda (lines stationName time)
        (map (lambda (line)
               (list (line-get-id line)
                     (line-get-name line)
                     (line-get-rail-type line)
                     (map (lambda (section)
                            (list (if (eq? (station-get-name (section-get-point1 section)) stationName)
                                      (list (station-get-id (section-get-point1 section))
                                            (station-get-name (section-get-point1 section))
                                            (station-get-type (section-get-point1 section))
                                            time)
                                      (section-get-point1 section))
                                  (if (eq? (station-get-name (section-get-point2 section)) stationName)
                                      (list (station-get-id (section-get-point2 section))
                                            (station-get-name (section-get-point2 section))
                                            (station-get-type (section-get-point2 section))
                                            time)
                                      (section-get-point2 section))
                                  (section-get-distance section)
                                  (section-get-cost section)))
                      (line-get-section line))))
               lines)))
    (list (subway-get-id sub)
          (subway-get-nombre sub)
          (third sub)
          (subway-set-station-stoptime-int (fourth sub) stationName time)
          (fifth sub))))
    
;Cambiando el tiempo de parada de algunas estaciones
(define sw0e (subway-set-station-stoptime sw0d "Los Héroes" 180))
(define sw0f (subway-set-station-stoptime sw0e "San Pablo" 50))


;; Req25: TDA subway - Modificador

; Dom = sub (subway) X trainId (int) X lineID (int)
; Rec = subway

(define subway-assign-train-to-line
  (lambda (sub trainId lineID)
    (cond
      [(null? (list-tail sub 5)) (list (subway-get-id sub)
                                       (subway-get-nombre sub)
                                       (third sub)
                                       (fourth sub)
                                       (fifth sub)
                                       (list (list "idLine" lineID) (list "idTrain" trainId)))]
      [else (list (subway-get-id sub)
                  (subway-get-nombre sub)
                  (third sub)
                  (fourth sub)
                  (fifth sub)
                  (list (sixth sub) (list (list "idLine" lineID) (list "idTrain" trainId))))])))
    
;Asignando trenes a líneas
(define sw0g (subway-assign-train-to-line sw0f 0 1))
(define sw0h (subway-assign-train-to-line sw0g 2 2))


;; Req26: TDA subway - Modificador

; Dom = sub (subway) X driverId (int) X trainId (int) X departureTime(String en formato HH:MM:SS de 24 hrs) X departure-station (String) X arrival-station (String)
; Rec = subway
; Recursividad = Natural X Cola

(define subway-assign-driver-to-train
  (lambda (sub driverId trainId departureTime departure-station arrival-station)
     (define find-train
      (lambda (lst trainId)
        (cond
          [(null? lst) #f]
          [else (if (eq? (second (second (car lst))) trainId)
                    #t
                    (find-train (cdr lst) trainId))])))
    (define add-train
      (lambda (lst trainId driverId departureTime departure-station arrival-station acc)
        (cond
          [(null? lst) (reverse acc)]
          [else (if (eq? (second (second (car lst))) trainId)
                    (add-train (cdr lst)
                               trainId
                               driverId
                               departureTime
                               departure-station
                               arrival-station
                               (cons (append (car lst) (list (list "idDriver" driverId) (list departureTime departure-station arrival-station))) acc))
                    (add-train (cdr lst) trainId driverId departureTime departure-station arrival-station (cons (car lst) acc)))])))
    (cond
      [(find-train (sixth sub) trainId) (list (subway-get-id sub)
                                              (subway-get-nombre sub)
                                              (third sub)
                                              (fourth sub)
                                              (fifth sub)
                                              (add-train (sixth sub) trainId driverId departureTime departure-station arrival-station null))]
      [else sub])))
    
;Asignando conductores a trenes
(define sw0i (subway-assign-driver-to-train sw0h 0 0 "11:00:00" "San Pablo" "Los Héroes"))
(define sw0j (subway-assign-driver-to-train sw0i 2 2 "12:00:00" "El Llano" "Toesca"))


;; Req27: TDA subway - Otras funciones

; Dom = sub (subway) X trainId (int) X time (String en formato HH:MM:SS d 24 hrs)
; Rec = station
; Recursion = Cola X Natural


; Funcion que busca una linea en un subway
(define find-line-in-subway
  (lambda (sub trainId)
    (define find-idLine
      (lambda (lst trainId)
        (cond
          [(null? lst) null]
          [else (if (eq? (second (second (car lst))) trainId)
                (second (first (car lst)))
                (find-idLine (cdr lst) trainId))])))
    (define find-line
      (lambda (lst lineId)
        (cond
          [(null? lst) null]
          [else (if (eq? (caar lst) lineId)
                    (car lst)
                    (find-line (cdr lst) lineId))])))
    (find-line (fourth sub) (find-idLine (sixth sub) trainId))))

; Funcion que calcula el tiempo total disponible en segundos
(define calculate-difference-time
  (lambda (timeStart timeEnd)
    (- (+ (* (string->number (substring timeEnd 0 2)) 3600) (* (string->number (substring timeEnd 3 5)) 60) (string->number (substring timeEnd 6 8)))
       (+ (* (string->number (substring timeStart 0 2)) 3600) (* (string->number (substring timeStart 3 5)) 60) (string->number (substring timeStart 6 8))))))

; Funcion que obtiene la hora de salida de un tren
(define time-start
  (lambda (lst trainId)
    (cond
      [(null? lst) null]
      [else (if (eq? (second (second (car lst))) trainId)
                (first (fourth (car lst)))
                (time-start (cdr lst) trainId))])))

; Funcion que obtien la estacion inicial de un tren
(define start-station
  (lambda (lst trainId)
    (cond
      [(null? lst) null]
      [else (if (eq? (second (second (car lst))) trainId)
                (second (fourth (car lst)))
                (start-station (cdr lst) trainId))])))
    

; Funcion que obtiene la estacion final de un tren
(define end-station
  (lambda (lst trainId)
    (cond
      [(null? lst) null]
      [else (if (eq? (second (second (car lst))) trainId)
                (third (fourth (car lst)))
                (start-station (cdr lst) trainId))])))

(define where-is-train
  (lambda (sub trainId time)
    (define where-is-train-int
      (lambda (lst totalTime currentStation endStation)
        (cond
          [(null? lst) currentStation]
          [else (if (and (eq? (station-get-name (section-get-point1 (car lst))) currentStation)
                         (not (eq? (station-get-name (section-get-point2 (car lst))) endStation)))
                    (if (> totalTime (+ (station-get-stop-time (section-get-point2 (car lst))) (/ (* (section-get-distance (car lst)) 1000) 16.67)))
                        (where-is-train-int (cdr lst)
                                            (- totalTime (+ (station-get-stop-time (section-get-point2 (car lst))) (/ (* (section-get-distance (car lst)) 1000) 16.67)))
                                            (station-get-name (section-get-point2 (car lst)))
                                            endStation)
                        (section-get-point1 (car lst)))
                    (section-get-point2 (car lst)))])))
    (where-is-train-int (line-get-section (find-line-in-subway sub trainId))
                        (calculate-difference-time (time-start (sixth sub) trainId) time)
                        (start-station (sixth sub) trainId)
                        (end-station (sixth sub) trainId))))

;preguntando dónde está el tren
(where-is-train sw0j 0 "11:12:00")  ;Debería estar mas cerca de Las Rejas. Hasta esta hora el tren debería haber recorrido 12km (asumiendo esta unidad), sumando los tiempos de parada en las estaciones



    