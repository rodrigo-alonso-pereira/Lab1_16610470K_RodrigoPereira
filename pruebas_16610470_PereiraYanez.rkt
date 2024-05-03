#lang racket

(require "tdaStation_16610470_PereiraYanez.rkt")
(require "tdaLine_16610470_PereiraYanez.rkt")
(require "tdaSection_16610470_PereiraYanez.rkt")
(require "tdaPcar_16610470_PereiraYanez.rkt")
(require "tdaTrain_16610470_PereiraYanez.rkt")
(require "tdaSubway_16610470_PereiraYanez.rkt")
(require "main_16610470_PereiraYanez.rkt")

;Req 2: TDA station - constructor
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


;Req 3: TDA section - constructor
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


;Req 4: TDA line - constructor
;creación de Línea 1 con todos los tramos
(define l1 (line 1 "Línea 1" "UIC 60 ASCE" s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15))
;creación de Línea 2 sin incluir tramos
(define l2 (line 2 "Línea 2" "100 R.E."))


;Req 5: TDA line - otras funciones
;obteniendo distancia total de linea
(line-lenght l1) ;resultado debe ser 64,3 si considera inclusive los tramos hacia estaciones de mantenimiento 
(line-lenght l2) ;resultado debe ser 0


;Req 6: TDA line - otras funciones
;obteniendo distancia entre estaciones
(line-section-length l1 "San Pablo" "Las Rejas") ;respuesta es 9.5


;Req 7: TDA line - otras funciones
;obteniendo costos de lineas
(line-cost l1) ;resultado debe ser 246 si considera inclusive los tramos hacia estaciones de mantenimiento 
(line-cost l2) ;resultado debe ser 0


;Req 8: TDA line - otras funciones
;obteniendo costos entre estaciones
(line-section-cost l1 "San Pablo" "Las Rejas") ;respuesta es 39


;Req 9: TDA line - modificador
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


;Req10: TDA Línea - pertenencia
;validando lineas
(line? l1)  ;devuelve true
(line? l2)  ;devuelve false
(line? l2e)  ;devuelve false
(line? l2h)  ;devuelve true


;Req11: TDA pcar - Constructor
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


;Req12: TDA train - Constructor
;creando trenes
(define t0 (train 0 "CAF" "UIC 60 ASCE" 60 1.5)) ;tren sin carros definidos
(define t1 (train 1 "CAF" "UIC 60 ASCE" 70  2 pc0 pc1 pc2 pc3 pc4)) ;tren válido
(define t2 (train 2 "CAF" "100 R.E." 70  2 pc5 pc6 pc7 pc8 pc9)) ;tren válido
(define t3 (train 3 "CAF" "100 R.E." 70  2 pc11a pc11 pc12 pc13 pc14 pc15 pc16 pc17)) ;tren válido
(define t4 (train 4 "CAF" "100 R.E." 70  2 pc1 pc2 pc3)) ;tren inválido sin terminales en extremos
(define t5 (train 5 "CAF" "100 R.E." 70  2 pc0 pc5 pc9 pc12 pc17))  ;tren inválido por incompatibilidad de carros


;Req13: TDA train - Modificador
;agregando carros a convoy
(define t0a (train-add-car t0 pc5 0))
(define t0b (train-add-car t0a pc6 1))
(define t0c (train-add-car t0b pc7 2))
(define t0d (train-add-car t0c pc8 3))
(define t0e (train-add-car t0d pc9 4)) ;tren válido


;Req14: TDA train - Modificador
;removiendo carros a convoy
(define t1a (train-remove-car t1 0))
(define t1b (train-remove-car t1 2))


;Req15: TDA train - Pertenencia
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


;Req16: TDA train - Otras funciones
;determinar capacidad del tren
(train-capacity t0) ;debe arrojar 0
(train-capacity t1) ;debe arrojar 550


;Req17: TDA driver - Constructor
;Creando drivers
(define d0 (driver 0 "Juan" "CAF"))
(define d1 (driver 1 "Alejandro" "Alsthom"))
(define d2 (driver 2 "Diego" "Alsthom"))
(define d3 (driver 3 "Pedro" "CAF"))


;Req18: TDA subway - Constructor
;Creando subway
(define sw0 (subway 0 "Metro de Santiago"))
(define sw1 (subway 1 "Subte"))


;Req19: TDA subway - Modificador
;Agregando trenes
(define sw0a (subway-add-train sw0 t1 t2 t0e))


;Req20: TDA subway - Modificador
;Agregando lineas
(define sw0b (subway-add-line sw0a l1 l2h))


;Req21: TDA subway - Modificador
;Agregando drivers
(define sw0c (subway-add-driver sw0b d0 d1 d2 d3))


;Req22: TDA subway - Otras funciones
;Expresado subway como string
(subway->string sw0c)


;Req23: TDA subway - Modificador
;Aumentando los costos de las estaciones en un 30%
(define sw0d (subway-rise-section-cost sw0c (lambda (c) (* c 1.3))))


;Req24: TDA subway - Modificador
;Cambiando el tiempo de parada de algunas estaciones
(define sw0e (subway-set-station-stoptime sw0d "Los Héroes" 180))
(define sw0f (subway-set-station-stoptime sw0e "San Pablo" 50))


;Req25: TDA subway - Modificador
;Asignando trenes a líneas
(define sw0g (subway-assign-train-to-line sw0f 0 1))
(define sw0h (subway-assign-train-to-line sw0g 2 2))


;Req26: TDA subway - Modificador
;Asignando conductores a trenes
(define sw0i (subway-assign-driver-to-train sw0h 0 0 "11:00:00" "San Pablo" "Los Héroes"))
(define sw0j (subway-assign-driver-to-train sw0i 2 2 "12:00:00" "El Llano" "Toesca"))


;Req27: TDA subway - Otras funciones
;preguntando dónde está el tren
(where-is-train sw0j 0 "11:12:00") ;'(3 "Las Rejas" "Regular" 45)

;Req28: TDA subway - Otras funciones
;produciendo la ruta que sigue el tren
(subway-train-path sw0j 0 "11:30:00") ;'("San Pablo" "Neptuno" "Pajaritos" "Las Rejas" "Ecuador" "San Alberto Hurtado" "Universidad de Santiago de Chile")

(newline)
(writeln "-------------------------- EJEMPLOS --------------------------")
(newline)


;Req 2: TDA station - constructor
;Creando estaciones L3 simplificada metro valparaiso
(define e30 (station 0 "Puerto" t 90))
(define e31 (station 1 "Bellavista" r 45))
(define e32 (station 2 "Francia" c 45))
(define e33 (station 3 "Baron" r 45))
(define e34 (station 4 "Portales" r 60))
(define e35 (station 5 "Recreo" t 40))
(define e36 (station 6 "Miramar" t 40))

;Creando estaciones L4 simplificada metro santiago, para una versión circular
(define e37 (station 7 "Viña del Mar" r 60))
(define e38 (station 8 "Hospital" r 50))
(define e39 (station 9 "Chorrillos" r 55))
(define e40 (station 10 "El Salto" r 55))
(define e41 (station 11 "Quilpue" r 55))


;Req 3: TDA section - constructor
;Creando tramos Línea 3 y 5
(define s30 (section e30 e31 5 5))
(define s31 (section e31 e32 6 4))
(define s32 (section e32 e33 1.5 11))
(define s33 (section e33 e34 2.5 20))
(define s34 (section e34 e35 4.5 24))
(define s35 (section e35 e36 3.3 10))

;Creando tramos Línea 4, línea circular
(define s36 (section e37 e38 5 11))
(define s37 (section e38 e39 2 16))
(define s38 (section e39 e40 4 18))
(define s39 (section e40 e41 9.5 6))


;Req 4: TDA line - constructor
;creación de Línea 3 y 5 con todos los tramos
(define l3 (line 3 "Línea 3" "UIC 60 ASCE" s30 s31 s32 s33 s34 s35))
(define l5 (line 5 "Línea 3" "UIC 60 ASCE" s30 s31 s33 s34 s35))
;creación de Línea 2 sin incluir tramos
(define l4 (line 4 "Línea 4" "100 R.E."))


;Req 5: TDA line - otras funciones
;obteniendo distancia total de linea
(line-lenght l3) ;resultado debe ser 22.8
(line-lenght l4) ;resultado debe ser 0
(line-lenght l5) ;resultado debe ser 21.3


;Req 6: TDA line - otras funciones
;obteniendo distancia entre estaciones
(line-section-length l3 "Puerto" "Recreo") ;respuesta es 19.5
(line-section-length l5 "Puerto" "Baron") ;respuesta es 21.3
(line-section-length l5 "Baron" "Miramar") ;respuesta es 10.3


;Req 7: TDA line - otras funciones
;obteniendo costos de lineas
(line-cost l3) ;resultado debe ser 74
(line-cost l4) ;resultado debe ser 0
(line-cost l5) ;resultado debe ser 63


;Req 8: TDA line - otras funciones
;obteniendo costos entre estaciones
(line-section-cost l3 "Puerto" "Francia") ;respuesta es 9
(line-section-cost l5 "Puerto" "Recreo") ;respuesta es 53
(line-section-cost l5 "Bellavista" "Portales") ;respuesta es 24


;Req 9: TDA line - modificador
;añadiendo tramos a l2
(define l4a (line-add-section l4 s36))
(define l4b (line-add-section l2a s37))
(define l4c (line-add-section l2b s38))


;Req10: TDA Línea - pertenencia
;validando lineas
(line? l3)  ;devuelve true
(line? l4)  ;devuelve false
(line? l5)  ;devuelve false


;Req11: TDA pcar - Constructor
;creando carros
(define pc30 (pcar 30 100 "NS-74" tr))
(define pc31 (pcar 31 100 "NS-74" ct))
(define pc32 (pcar 32 150 "NS-74" ct))
(define pc33 (pcar 33 100 "NS-74" ct))
(define pc34 (pcar 34 100 "NS-74" tr))


;Req12: TDA train - Constructor
;creando trenes
(define t30 (train 30 "CAF" "UIC 60 ASCE" 60 1.5)) ;tren sin carros definidos
(define t31 (train 31 "CAF" "UIC 60 ASCE" 70  2 pc30 pc31 pc32 pc33 pc34)) ;tren válido
(define t32 (train 32 "CAF" "100 R.E." 70  2 pc30 pc31 pc32 pc33)) ;tren inválido


;Req13: TDA train - Modificador
;agregando carros a convoy
(define t30a (train-add-car t30 pc30 0))
(define t30b (train-add-car t30a pc31 1))
(define t30c (train-add-car t30b pc32 2))


;Req14: TDA train - Modificador
;removiendo carros a convoy
(define t31a (train-remove-car t31 0))
(define t31b (train-remove-car t31 2))
(define t31c (train-remove-car t31 1))


;Req15: TDA train - Pertenencia
;verificación de válidez en la conformación de trenes
(train? t30) ;debe arrojar #f
(train? t31) ;debe arrojar #t
(train? t32) ;debe arrojar #f


;Req16: TDA train - Otras funciones
;determinar capacidad del tren
(train-capacity t30) ;debe arrojar 0
(train-capacity t31) ;debe arrojar 550
(train-capacity t32) ;debe arrojar 0 (no se agregaron pcar por no tener orden correcto)


;Req17: TDA driver - Constructor
;Creando drivers
(define d30 (driver 30 "Pedro" "CAF"))
(define d31 (driver 31 "Juan" "Alsthom"))
(define d32 (driver 32 "Diego" "CAF"))


;Req18: TDA subway - Constructor
;Creando subway
(define sw30 (subway 30 "Metro de Valparaiso"))
(define sw31 (subway 31 "Metro de Viña del Mar"))
(define sw32 (subway 32 "Metro Circular Costanera"))


;Req19: TDA subway - Modificador
;Agregando trenes
(define sw30a (subway-add-train sw0 t31 t32 t30a))


;Req20: TDA subway - Modificador
;Agregando lineas
(define sw30b (subway-add-line sw30a l3 l4))


;Req21: TDA subway - Modificador
;Agregando drivers
(define sw30c (subway-add-driver sw30b d30 d31 d32))


;Req22: TDA subway - Otras funciones
;Expresado subway como string
(subway->string sw30c)


;Req23: TDA subway - Modificador
;Aumentando los costos de las estaciones en un 30%
(define sw30d (subway-rise-section-cost sw30c (lambda (c) (* c 1.4))))


;Req24: TDA subway - Modificador
;Cambiando el tiempo de parada de algunas estaciones
(define sw30e (subway-set-station-stoptime sw30d "Puerto" 180))
(define sw30f (subway-set-station-stoptime sw30e "Francia" 50))
(define sw30g (subway-set-station-stoptime sw30f "Recreo" 150))


;Req25: TDA subway - Modificador
;Asignando trenes a líneas
(define sw30h (subway-assign-train-to-line sw30g 30 3))
(define sw30i (subway-assign-train-to-line sw30h 31 4))


;Req26: TDA subway - Modificador
;Asignando conductores a trenes
(define sw30j (subway-assign-driver-to-train sw30i 30 30 "10:00:00" "Puerto" "Miramar"))
(define sw30k (subway-assign-driver-to-train sw30j 32 31 "11:00:00" "Francia" "Miramar"))


;Req27: TDA subway - Otras funciones
;preguntando dónde está el tren
(where-is-train sw30k 30 "10:15:00") ;'(3 "Baron" "Regular" 45)


;Req28: TDA subway - Otras funciones
;produciendo la ruta que sigue el tren
(subway-train-path sw30k 30 "10:16:00") ;'("Puerto" "Bellavista" "Francia" "Baron")
