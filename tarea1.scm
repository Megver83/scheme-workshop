; Tarea 1
; https://replit.com/@LUISJERONIMO2/Tarea1
; https://github.com/Megver83/scheme-workshop
; Integrantes: David Pizarro, Luis Jeronimo, Felipe Gallardo, Daniel Olivares

; Defino "suma" para utilizar en el ejercicio 18 y 13
(define (suma lst)
    (cond
        ((null? lst) 0)
        ((not (pair? lst)) (car lst))
        (else (+ (car lst) (suma (cdr lst))))
    )
)

; 1. Haga una función que vaya restando de izquierda a derecha todos los números de una lista.
;     Ejemplo ‘(1 2 3) = -1 -2 -3 = -6

; En el ejemplo, por lo que veo, simplemente les cambia el signo a los numeros y los suma.
; A mi modo de ver, restar los numeros de izquierda a derecha sería ‘(1 2 3) = 1 - 2 - 3 = -4
; pero en mi funcion voy a guiarme por el ejemplo

(define (resta-izq-der lista)
    (if (null? lista)
        0
        (+ (- (car lista)) (resta-izq-der (cdr lista)))
    )
)

(display "Ejecución ejercicio 1: ")
(display (resta-izq-der '(1 2 3)))
(newline)

; 2. Haga una función que devuelva el número negativo de cada número de una lista.

(define (neg-nums lista) (map (lambda(x) (- x)) lista))

(display "Ejecución ejercicio 2: ")
(display (neg-nums '(1 2 3)))
(newline)

; 3. Haga una función que reciba (x y) numéricos. Esta debe entregar el cuadrante en que se
;    encuentran esas coordenadas.

(define (cuadrante x y)
    (cond
        ((and (> x 0) (> y 0)) 1)
        ((and (< x 0) (> y 0)) 2)
        ((and (< x 0) (< y 0)) 3)
        ((and (> x 0) (< y 0)) 4)
    )
)

(display "Ejecución ejercicio 3:")
(newline)
(display "* Cuadrante (1, 2): ")
(display (cuadrante 1 2))
(newline)
(display "* Cuadrante (-1, 2): ")
(display (cuadrante -1 2))
(newline)
(display "* Cuadrante (-1, -2): ")
(display (cuadrante -1 -2))
(newline)
(display "* Cuadrante (1, -2): ")
(display (cuadrante 1 -2))
(newline)

; 4. Haga una función que cuente la longitud de un String.

; (string-length "string")
(define (longitud cadena)
    (if (equal? cadena "")
        0
        (+ 1 (longitud (substring cadena 1)))
    )
)

(display "Ejecución ejercicio 4: ")
(display (longitud "string de largo 18"))
(newline)

; 5. Haga una función que cuente la cantidad de elementos de una lista sin usar (length )

(define (len lista)
    (if (null? lista)
        0
        (+ 1 (len (cdr lista)))
    )
)

(display "Ejecución ejercicio 5: ")
(display (len '(1 2 3)))
(newline)

; 6. Haga una función que calcule la potencia de un número sin usar (expt ).

(define (ex base potencia)
    (letrec ((multi-n-veces (lambda(n)
        (if (= n 0)
            1
            (* base (multi-n-veces (- n 1)))
        )
        )))
        (cond
            ((and (> potencia 0) (integer? potencia)) (multi-n-veces potencia))
            ((and (< potencia 0) (integer? potencia)) (/ 1 (multi-n-veces (- potencia))))
            ((and (= potencia 0)) (multi-n-veces potencia))
            (else "Argumentos invalidos, revisa que pusiste como base y/o potencia")
        )
    )
)

(display "Ejecución ejercicio 5: ")
(display (ex 2 4))
(newline)

; 7. Haga una función que reciba una lista de números, y que retorne una lista con esos números
;    multiplicados por 2.

(define (doble lista)
    (map (lambda(x) (* x 2)) lista)
)

(display "Ejecución ejercicio 7: ")
(display (doble '(1 2 3)))
(newline)

; 8. Haga una función que reciba una lista de números, y que retorne los números pares de esa
;    lista sumados con 2.

(define (pares+2 lista)
    ; No estoy seguro si lo que pide es la lista con los mismos numeros, sumando 2 a los pares,
    ; o una lista solamente con los pares + 2
    ; Si lo que pedía era el primer caso, la función sería la misma pero sin filter
    (filter (lambda(x) (even? x))
        (map (lambda(x) (if (even? x) (+ x 2) x)) lista)
    )
)

(display "Ejecución ejercicio 8: ")
(display (pares+2 '(1 2 3 4 5 6)))
(newline)

; 11. Haga una función que sirva para comprobar si un número es primo.

(define (es-primo? n)
    (and (> n 1)
        (letrec ((iterar (lambda (i)
            (or (= i n)
                (and (not (= (modulo n i) 0))
                    (iterar (+ i 1)))))))
            (iterar 2)
        )
    )
)

; 9. Haga una función que retorne una lista con los primeros N números primos.

(define (generar-primos cant)
    (letrec (
        (agregar-primo (lambda(n lista)
            (if (es-primo? n)
                (append lista (list n))
                lista
            ))
        )
        (iterar (lambda(n lista)
            (if (= (length lista) cant)
                lista
                (iterar (+ n 1) (agregar-primo n lista))
            )))
        )
        ; inicializa en el 2, el primer número primo
        (iterar 2 '())
    )
)

(display "Ejecución ejercicio 9: ")
(display (generar-primos 5))
(newline)

; 10. Haga una función que filtre los números que son resultados de un factorial de 1 a 10 (1 (1!), 2
;     (2!), 6 (3!), 24 (4!), etc) de una lista. Ejemplo: ‘(1 2 3 4 5 6) -> ‘(3 4 5)
; NOTA: El profesor me dijo en clase que era retornar una lista de los que son factoriales de 3 a 10

(define (filt-fact lista)
    (letrec ((factoriales '(6 24 120 720 5040 40320 362880 3628800))
        (presente (lambda(x lst) (not (not (member x lst))))))
        (filter (lambda(n) (presente n factoriales)) lista)
    )
)

(display "Ejecución ejercicio 10: ")
(display (filt-fact '(1 120 2 3 24 4 5 6 7)))
(newline)

; 12. Haga una función que aplane (es decir, “flat”) una lista. Ejemplo: ( 1 (12 23)) pasa a ser (1 12 23).

(define (flatten lst)
    (cond
        ((null? lst) '())
        ((not (pair? lst)) (list lst))
        (else (append (flatten (car lst)) (flatten (cdr lst))))
    )
)

(display "Ejecución ejercicio 12: ")
(display (flatten  '(1 (12 23) (2 5 8))))
(newline)

; 13. Haga una función que calcule un promedio de notas a partir de una lista (Todas las notas con
;     la misma ponderación).

(define (media lista) (/ (suma lista) (length lista)))

(display "Ejecución ejercicio 13: ")
(display (media '(1 2 3)))
(newline)

; 14. Haga una función que retorne los n últimos elementos de una lista, ejemplo: '(1 2 3 4 5) => ‘(4 5)

(define (last-n n lista)
    (if (= (length lista) n)
        lista
        (last-n n (cdr lista))
    )
)

(display "Ejecución ejercicio 14: ")
(display (last-n 2 '(1 2 3 4 5)))
(newline)

; 15. Haga la función "full-comparar" que dada dos listas devuelve verdadero si es que contienen
;     los mismos elementos en el mismo orden (ignorando paréntesis) y retornar falso en otro caso,
;     ejemplo: (full-comparar '(4 (5 6)) '((4 5) 6)) => #t (full-comparar '(3 2 1) '(1 (3 2)) => #f . En otras
;     palabras, la lista aplanada de ambas listas debe ser igual en el mismo orden.

(define (full-comparar l1 l2)
    (let ((lista1 (flatten l1)) (lista2 (flatten l2)))
        (cond
            ; Si ambas listas son vacias, las consideramos iguales
            ((and (null? lista1) (null? lista2)) #t)
            ; Si solo una lista es vacia, ya no son iguales, entonces regresamos #false
            ((or (null? lista1) (null? lista2)) #f)
            ; Si el primer elemento de ambas listas no es igual, entonces las listas no son iguales. Devolvemos #false.
            ((not (= (car lista1) (car lista2))) #f)
            ; Si el primer elemento es igual, llamamos recursivamente a la función con las colas de las listas.
            (else (full-comparar (cdr lista1) (cdr lista2)))
        )
    )
)

(display "Ejecución ejercicio 15: ")
(display (full-comparar '(4 (5 6)) '((4 5) 6)))
(newline)

; 16. Haga la función para calcular la discriminante de una ecuación cuadrática.

(define (discriminante a b c) (- (ex b 2) (* 4 a c)))

(display "Ejecución ejercicio 16: ")
(display (discriminante 1 2 1))
(newline)

; 17. Haga una función que añade el símbolo “!” a cada elemento de una lista de strings.

(define (concat! lst)
    ; No sé si hay que agregarlo al inicio o al final, lo hice al inicio
    ; En el otro caso, simplemente hay que invertir el orden de los argumentos de string-append
    (map (lambda(x) (string-append "!" x)) lst)
)

(display "Ejecución ejercicio 17: ")
(display (concat! '("Hola" "mundo" "!")))
(newline)

; 18. Haga una función que sume los números que son impares de una lista.

(define (suma-impares lst)
    (suma (filter (lambda(x) (not (even? x))) lst))
)

(display "Ejecución ejercicio 18: ")
(display (suma-impares '(1 2 3 4 5 6)))
(newline)
