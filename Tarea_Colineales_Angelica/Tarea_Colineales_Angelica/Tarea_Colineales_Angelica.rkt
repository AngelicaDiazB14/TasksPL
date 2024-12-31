; ...............................................................................................
; Tecnológico de Costa Rica                                                                     .
; Lenguajes de Programación, GR 40                                                              .
; Tarea programada 1: Colineales                                                                .
; Realizado por: Angélica Díaz Barrios                                                          .
;                                                                                               .
; Este programa recibe una lista con dos sublistas que representan dos segmentos,               .
; dentro de cada uno de estos segmentos se encuentran dos sublistas que representan             .
; los puntos con las coordenadas "x" y "y" respectivamente. La función principal                .
; "revise" se encarga de buscar las intersecciones y la clasificación de los                    .
; segmentos tales como: colineales, distinta, indefinida o paralelos                            .
; ...............................................................................................


; =============================================================================================== 
; Se definen las siguientes variables globales y se inicializan como false

; Puntos del primer segmento
(define x1 #f) 
(define y1 #f)
(define x2 #f) 
(define y2 #f)
; Puntos del segundo segmento
(define x3 #f) 
(define y3 #f)
(define x4 #f) 
(define y4 #f)
; Pendientes
(define m1 #f)
(define m2 #f)
; Valores b
(define b1 #f)
(define b2 #f)



; ===============================================================================================
;                                           revise
; ===============================================================================================

; Función principal que recibe una lista de segmentos, llama a extraer coordenadas y a calcular
; su pendiente, esta función invocará al resto de funciones.


(define (revise lista)
  (extraerCoordenadas lista) 
  (newline)
  (calcularPendiente))



; ===============================================================================================
;                                      extraerCoordenadas
; ===============================================================================================

; Esta función permite extraer las coordenadas de ambos segmentos y los almacena en las variables
; globales respectivas.


(define (extraerCoordenadas lista)       
  (let* ((puntoA (caar lista))        ; caar: primer elemento del primer elemento de la lista
         (puntoB (cadar lista))       ; cadar:primer elemento del segundo elemento de la lista
         (puntoC (caadr lista))       ; caadr: segundo elemento del primer elemento del segundo elemento de la lista
         (puntoD (cadadr lista)))     ; cadadr: segundo elemento del segundo elemento de la lista
    (set! x1 (car puntoA))            ; Establece el valor de x1 como el primer elemento de puntoA
    (set! y1 (cadr puntoA))           ; Establece el valor de y1 como el segundo elemento de puntoA
    (set! x2 (car puntoB))            ; Establece el valor de x2 como el primer elemento de puntoB
    (set! y2 (cadr puntoB))           ; Establece el valor de y2 como el segundo elemento de puntoB
    (set! x3 (car puntoC))            ; Establece el valor de x3 como el primer elemento de puntoC
    (set! y3 (cadr puntoC))           ; Establece el valor de y3 como el segundo elemento de puntoC
    (set! x4 (car puntoD))            ; Establece el valor de x4 como el primer elemento de puntoD
    (set! y4 (cadr puntoD))))         ; Establece el valor de y4 como el segundo elemento de puntoD




; ===============================================================================================
;                                        calcularPendiente
; ===============================================================================================

; Calcula las pendientes de cada uno de los segmentos y los compara para realizar los cálculos
; correspondientes, según cada caso.


(define (calcularPendiente)
  (set! m1                           
    (if (= x1 x2)
        'x                           ; Se asigna una x cuando los denominadores son iguales
        (/ (- y2 y1) (- x2 x1))))    ; Calcula la pendiente del primer segmento y la almacena en m1
  (set! m2
    (if (= x3 x4)                    
        'x 
        (/ (- y4 y3) (- x4 x3))))    ; Calcula la pendiente del segundo segmento y la almacena en m2
    (if (or (eq? m1 'x) (eq? m2 'x)) ; Se compara si alguna pendiente es x (indefinida)
        (imprimirResultado (calcular_Inter_indefinidas)); Si es así llama a calcular las intersecciones
        (verificarIguales)))         ; si no son indefinidas verificará si son pendientes iguales





; ===============================================================================================
;                                      verificarIguales
; ===============================================================================================

; Verifica si las pendientes son iguales, si es así llama a verificar si son colineales o paralelos
; si no lo son invoca a calcular las intersecciones de distinta pendiente.


(define (verificarIguales)
  (if (eq? m1 m2)
      (verificar_Coli_Paralelo)
      (calcular_Inter_distintaPen)))




; ===============================================================================================
;                                      imprimirResultado
; ===============================================================================================

; Permite imprimir los resultados, pero solo se utiliza para los resultados de calcular las
; intersecciones indefinidas.


(define (imprimirResultado resultado)
  (cond
    ((equal? resultado "(Indefinida F F (no hay intersección))") (display resultado))
    (else 
     (let ((mensaje (car resultado)) ; Define mensaje como el primer elemento del resultado
           (segmento1 (cadr resultado)); Define segmento1 como el segundo elemento del resultado
           (segmento2 (caddr resultado)); Define segmento2 como el tercer elemento del resultado
           (punto (cadddr resultado))); Define punto como el cuarto elemento del resultado
       (display "(")                  ; Imprime los componentes del resultado formateados
       (display mensaje) 
       (display " ")
       (display segmento1)
       (display " ")
       (display segmento2)
       (display " ")
       (display punto)
       (display ")")))))



; ===============================================================================================
;                                   calcular_Inter_indefinidas
; ===============================================================================================

; Permite verificar si existen intersecciones en los segmentos indefinidos y si tienen intersección
; indica en cúal o cuales segmentos está contenida la intersección y el punto donde intersecan.


(define (calcular_Inter_indefinidas)
  ; Define una función llamada calcular_Inter_indefinidas
  (cond
    ; Si ambas pendientes son 'x' y comparten coordenadas, retorna el resultado adecuado
    ((and (eq? m1 'x) (eq? m2 'x) (compartenCoordenadas))
     (list "Indefinida" "T" "T" (list (list x1 y1))))
    ; Si la pendiente m1 es 'x' y el punto de intersección está dentro del segmento 3-4, retorna el resultado adecuado
    ((eq? m1 'x) (if (and (>= y3 (min y1 y2)) (<= y3 (max y1 y2)) (>= x1 (min x3 x4)) (<= x1 (max x3 x4)))
                    (list "Indefinida" "T" "F" (list (list x1 y3)))
                    "(Indefinida F F (no hay intersección))"))
    ; Si la pendiente m2 es 'x' y el punto de intersección está dentro del segmento 1-2, retorna el resultado adecuado
    ((eq? m2 'x) (if (and (>= y1 (min y3 y4)) (<= y1 (max y3 y4)) (>= x3 (min x1 x2)) (<= x3 (max x1 x2)))
                    (list "Indefinida" "F" "T" (list (list x3 y1)))
                    "(Indefinida F F (no hay intersección))"))
    ; Si la pendiente m1 es 'x' y comparten coordenadas, retorna el resultado adecuado
    ((and (eq? m1 'x) (compartenCoordenadas))
     (if (or (= x1 x3) (= x1 x4))
         (list "Indefinida" "T" "T" (list (list x1 y1)))
         "(Indefinida F F (no hay intersección))"))
    ; Si la pendiente m2 es 'x' y comparten coordenadas, retorna el resultado adecuado
    ((and (eq? m2 'x) (compartenCoordenadas))
     (if (or (= x3 x1) (= x3 x2))
         (list "Indefinida" "T" "T" (list (list x3 y3)))
         "(Indefinida F F (no hay intersección))"))
    ; Si no se cumple ninguna de las condiciones anteriores, retorna un resultado predeterminado
    (else (list "Indefinida" "T" "T" (list (list x1 y1))))))






; ===============================================================================================
;                                      compartenCoordenadas
; ===============================================================================================

 ; Retorna verdadero si alguno de los puntos comparte coordenadas

(define (compartenCoordenadas)
  (or (= x1 x3)
      (= x1 x4)
      (= x2 x3)
      (= x2 x4)))


; ===============================================================================================
;                                          calcular_b
; ===============================================================================================


(define (calcular_b)
  (set! b1 (- y1 (* m1 x1)))         ; Calcular b1 para el primer segmento
  (set! b2 (- y3 (* m2 x3))))        ; Calcular b2 para el segundo segmento



; ===============================================================================================
;                                     verificar_Coli_Paralelo
; ===============================================================================================



(define (verificar_Coli_Paralelo)
  (calcular_b)                       ; Llamar a la función para calcular b1 y b2
  (if (= b1 b2)                      ; Verificar si b1 y b2 son iguales
      (calcular_Inter_Colineales)    ; Si son iguales son colineales e invoca a la función correspondiente
      (display "(Paralelos F F (no hay intersección))"))); Si no son iguales, significa que son paralelos





; ===============================================================================================
;                                   calcular_Inter_Colineales
; ===============================================================================================

;Permite calcular las intersecciones de los segmentos que son colineales 

(define (calcular_Inter_Colineales)
  ; Verifica si hay intersección entre segmentos de línea colineales
  (if (verificarInterseccionSeg)
      ; Si hay intersección, realiza las siguientes operaciones
      (let ((interseccion '()))  ; Define una lista vacía para almacenar los puntos de intersección
        ; Verifica si el punto (x3, y3) está dentro del segmento 1-2 y lo agrega a la lista de intersección si lo está
        (if (verificarPuntoEnSeg (cons x3 y3) x1 y1 x2 y2)
            (set! interseccion (cons (list x3 y3) interseccion)))
        ; Verifica si el punto (x4, y4) está dentro del segmento 1-2 y lo agrega a la lista de intersección si lo está
        (if (verificarPuntoEnSeg (cons x4 y4) x1 y1 x2 y2)
            (if (not (member (list x4 y4) interseccion))
                (set! interseccion (cons (list x4 y4) interseccion))))
        ; Verifica si el punto (x1, y1) está dentro del segmento 3-4 y lo agrega a la lista de intersección si lo está
        (if (verificarPuntoEnSeg (cons x1 y1) x3 y3 x4 y4)
            (if (not (member (list x1 y1) interseccion))
                (set! interseccion (cons (list x1 y1) interseccion))))
        ; Verifica si el punto (x2, y2) está dentro del segmento 3-4 y lo agrega a la lista de intersección si lo está
        (if (verificarPuntoEnSeg (cons x2 y2) x3 y3 x4 y4)
            (if (not (member (list x2 y2) interseccion))
                (set! interseccion (cons (list x2 y2) interseccion))))
        ; Imprime los resultados
        (display "(Colineal ")
        (display (if (verificarPuntoEnSeg (cons x3 y3) x1 y1 x2 y2) "T" "F"))  ; Verifica si el punto (x3, y3) está dentro del segmento 1-2
        (display " ")
        (display (if (verificarPuntoEnSeg (cons x1 y1) x3 y3 x4 y4) "T" "F"))  ; Verifica si el punto (x1, y1) está dentro del segmento 3-4
        (display " (")
        ; Imprime los puntos de intersección
        (for-each (lambda (i)
                    (display "(")
                    (display (car i))
                    (display " ")
                    (display (cadr i))
                    (display ")"))
                  interseccion)
        (display "))"))  ; Imprime el resultado final
      ; Si no hay intersección, imprime un mensaje indicando la falta de intersección
      (display "(Colineal F F (no hay intersección))")))



; ===============================================================================================
;                                   verificarInterseccionSeg
; ===============================================================================================

; Verifica si hay intersección entre segmentos

(define (verificarInterseccionSeg)
  (or (and (verificarPuntoEnSeg (cons x3 y3) x1 y1 x2 y2)  ; Verifica si el punto (x3, y3) está dentro del segmento 1-2 y el punto (x4, y4) no lo está
           (not (verificarPuntoEnSeg (cons x4 y4) x1 y1 x2 y2)))  ; y el punto (x4, y4) está fuera del segmento 1-2
      (and (verificarPuntoEnSeg (cons x4 y4) x1 y1 x2 y2)  ; Verifica si el punto (x4, y4) está dentro del segmento 1-2 y el punto (x3, y3) no lo está
           (not (verificarPuntoEnSeg (cons x3 y3) x1 y1 x2 y2)))  ; y el punto (x3, y3) está fuera del segmento 1-2
      (and (verificarPuntoEnSeg (cons x1 y1) x3 y3 x4 y4)  ; Verifica si el punto (x1, y1) está dentro del segmento 3-4 y el punto (x2, y2) no lo está
           (not (verificarPuntoEnSeg (cons x2 y2) x3 y3 x4 y4)))  ; y el punto (x2, y2) está fuera del segmento 3-4
      (and (verificarPuntoEnSeg (cons x2 y2) x3 y3 x4 y4)  ; Verifica si el punto (x2, y2) está dentro del segmento 3-4 y el punto (x1, y1) no lo está
           (not (verificarPuntoEnSeg (cons x1 y1) x3 y3 x4 y4)))))  ; y el punto (x1, y1) está fuera del segmento 3-4



; ===============================================================================================
;                                     verificarPuntoEnSeg
; ===============================================================================================


 ; Verifica si un punto está dentro de un segmento

(define (verificarPuntoEnSeg punto x1 y1 x2 y2)
  ; Verifica si la coordenada x del punto está dentro del rango entre las coordenadas x1 y x2 del segmento
  (and (<= (car punto) (max x1 x2))
       (>= (car punto) (min x1 x2))
 ; Verifica si la coordenada y del punto está dentro del rango entre las coordenadas y1 y y2 del segmento
       (<= (cdr punto) (max y1 y2))
       (>= (cdr punto) (min y1 y2))))




; ===============================================================================================
;                                  calcular_Inter_distintaPen
; ===============================================================================================

; Permite calcular las intersecciones de los segmentos cuando tienen distinta pendiente.

(define (calcular_Inter_distintaPen)
  (calcular_b)
  (let ((coordenadaX (/ (- b2 b1) (- m1 m2))); Define variables locales para la coordenada "x" de la intersección
        (interseccion-y (+ (* m1 (/ (- b2 b1) (- m1 m2))) b1)));  y la coordenada "y" de la intersección
    (cond
      ; Si la intersección está dentro de ambos segmentos, imprime el resultado correspondiente
      ((and (>= coordenadaX x1) (<= coordenadaX x2)
            (>= coordenadaX x3) (<= coordenadaX x4))
       (display "(Distinta T T ")  ; Imprime la etiqueta de resultado para intersección dentro de ambos segmentos
       (display (list (if (integer? coordenadaX) (round coordenadaX) (redondear coordenadaX)) 
              (if (integer? interseccion-y) (round interseccion-y) (redondear interseccion-y))))  ; Imprime las coordenadas redondeadas de la intersección
       (display ")"))
      ; Si la intersección está dentro del segmento 1-2 pero no del segmento 3-4, imprime el resultado correspondiente
      ((and (>= coordenadaX x1) (<= coordenadaX x2))
       (display "(Distinta T F ")  ; Imprime la etiqueta de resultado para intersección dentro del segmento 1-2 pero no del segmento 3-4
       (display (list (if (integer? coordenadaX) (round coordenadaX) (redondear coordenadaX)) 
             (if (integer? interseccion-y) (round interseccion-y) (redondear interseccion-y))))  ; Imprime las coordenadas redondeadas de la intersección
       (display ")"))
      ; Si la intersección está dentro del segmento 3-4 pero no del segmento 1-2, imprime el resultado correspondiente
      ((and (>= coordenadaX x3) (<= coordenadaX x4))
       (display "(Distinta F T ")  ; Imprime la etiqueta de resultado para intersección dentro del segmento 3-4 pero no del segmento 1-2
       (display (list (if (integer? coordenadaX) (round coordenadaX) (redondear coordenadaX)) 
         (if (integer? interseccion-y) (round interseccion-y) (redondear interseccion-y))))  ; Imprime las coordenadas redondeadas de la intersección
       (display ")"))
      ; Si la intersección no está dentro de ninguno de los segmentos, imprime un mensaje indicando esto
      (else
       (display "(Distinta F F (la intersección no está contenida en los segmentos))")))))




; ===============================================================================================
;                                            redondear
; ===============================================================================================

; Permite redondear a dos decimales un resultado.

(define (redondear x)
  (/ (round (* x 100.0)) 100.0))




; ===============================================================================================
;                                        EJEMPLOS PARA PRUEBA
; ===============================================================================================

;               ___________________________________________________________________

;                                            PARALELOS:
;               ___________________________________________________________________

(display " ___________________________________________________________________")
(newline)
(display "                               PARALELOS")
(newline)

;NO TIENEN INTERSECCIÓN
(revise '(((1 1) (3 3)) ((4 1) (6 3))))
(newline)
;               ___________________________________________________________________

;                                          COLINEALES:
;               ___________________________________________________________________ 


(display " ___________________________________________________________________")
(newline)
(display "                               COLINEALES")
(newline)

;NO TIENEN INTERSECCIÓN
(revise '(((2 4) (5 7)) ((8 10) (11 13))))
(newline)

;SOLO COMPARTEN UN PUNTO DE INTERSECCIÓN Y ESTÁ EN EL PRIMER SEGMENTO:
(revise  '(((1 1) (3 3)) ((3 3) (5 5))))
(newline)

;SOLO COMPARTEN UN PUNTO DE INTERSECCIÓN Y ESTÁ EN EL SEGUNDO SEGMENTO:
(revise '(((3 3) (5 5)) ((1 1) (3 3))))
(newline)

;INTERSECCIONES ESTÁN EN EL PRIMER SEGMENTO:
(revise '(((1 1) (6 6)) ((3 3) (7 7))))
(newline)

;SON COLINEALES Y SUS PUNTOS DE INTERSECCIÓN SON (1 1) Y (2 2)
(revise '(((1 1) (5 5)) (( 2 2) (0 0))))
(newline)

;               ___________________________________________________________________

;                                      DISTINTA PEDIENTE:
;               ___________________________________________________________________ 


(display " ___________________________________________________________________")
(newline)
(display "                           DISTINTA PEDIENTE")
(newline)

;SE INTERSECAN EN AMBAS RECTAS
(revise '(((1 1) (4 4)) (( 2 2) (5 1))))
(newline)

;LA INTERSECCIÓN ESTÁ EN EL SEGUNDO SEGMENTO:
(revise '(((1 1) (2 2)) (( 4 6) (7 2))))
(newline)

;LA INTERSECCIÓN ESTÁ EN EL PRIMER SEGMENTO
(revise '(((4 6) (7 2)) (( 1 1) (2 2))))
(newline)

;LAS RECTAS TIENEN INTERSECCIÓN PERO NO ESTÁ CONTENIDA EN NINGÚN SEGMENTO:
(revise '(((1 1) (3 3)) (( 8 3) (5 5))))
(newline)


;               ___________________________________________________________________ 

;                                    PENDIENTE INDEFINIDA:
;               ___________________________________________________________________ 



(display " ___________________________________________________________________")
(newline)
(display "                          PENDIENTE INDEFINIDA")
(newline)

; INTERSECCIÓN ENTRE UN SEGMENTO VERTICAL Y UNO HORIZONTAL
(revise '(((1 2) (1 5)) ((0 4) (4 4))))
(newline)

; INTERSECCIÓN ENTRE UN SEGMENTO VERTICAL Y UNO HORIZONTAL CON LA INTERSECCIÓN EN EL PRIMER SEGMENTO
(revise '(((1 1) (1 5)) (( 0 3) (3 3))))
(newline)

; INTERSECCIÓN EN AMBOS SEGMENTOS
(revise '(((1 1) (1 5)) ((1 0) (1 1))))
(newline)


; INTERSECCIÓN EN EL SEGUNDO SEGMENTO
(revise '(((0 4) (4 1)) ((1 2) (1 5))))
(newline)

; INTERSECCIÓN EN AMBOS SEGMENTOS
(revise '(((1 2) (1 5)) ((1 0) (1 1))))
(newline)

; SIN INTERSECCIÓN
(revise '(((1 2) (1 5)) ((2 0) (2 2))))
(newline)

