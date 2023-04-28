;--------------------Ejercicio 1--------------------------
#|
(define (f1 lista)
  (if (> (length lista) 0) 
      (- (f1 (cdr lista)) (car lista))
      0))

(print "F1  "(f1 '(1 2 3)))

;--------------------Ejercicio 2--------------------------

(define (Negative l)
  (map (lambda (x) (* -1 x)) l))

(Negative '(1 2 3 4 5))

;--------------------Ejercicio 3-------------------------- 
(define (CCS x y)
  (cond ((and (> x 0) (> y 0)) "Primer cuadrante")
        ((and (< x 0) (> y 0)) "Segundo cuadrante")
        ((and (< x 0) (< y 0)) "Tercer cuadrante")
        ((and (> x 0) (< y 0)) "Cuarto cuadrante")
        (else "En el origen")))  ;
(CCS -3 4)
;--------------------Ejercicio 4--------------------------
(define Example "AISDJO ASJDO AI SJDO AS JDOIASD")
(define listExample (string->list Example))
(define (countList lst)
  (cond 
    [(null? lst) 0]
    [(char=? (car lst) #\space) (countList (cdr lst)) ]
    [(char? (car lst)) (+ 1 (countList (cdr lst)))]
    [else (countList (cdr lst))]))

(countList listExample)
;--------------------Ejercicio 5--------------------------

(define (f5 lista)
  (if(> (length lista) 0)
  (+ 1 (f5 (cdr lista)))
  0))

(print "F5  "(f5 '(1 2 3 4 5)))

;--------------------Ejercicio 6--------------------------
(define (potencia num pwr)
  (if (= pwr 0)
      1
      (* num )))
;--------------------Ejercicio 7--------------------------
|#
; ¿Por qué no usaron map? ~David
; (map (lambda(x) (* x 2)) lst)
(define (multiplicar lst)
  (if (null? lst)
      '()
      (cons (* 2 (car lst)) (multiplicar (cdr lst)))))
(multiplicar '(1 2 3 4 5))

;--------------------Ejercicio 8--------------------------
(define (pares+2 lista)
    ; No estoy seguro si lo que pide es la lista con los mismos numeros, sumando 2 a los pares,
    ; o una lista solamente con los pares + 2
    ; Si lo que pedía era el primer caso, la función sería la misma pero sin filter
    (filter (lambda(x) (even? x))
        (map (lambda(x) (if (even? x) (+ x 2) x)) lista)
    )
)
; ~David

(display "Ejecución ejercicio 8: ")
(display (pares+2 '(1 2 3 4 5 6)))
(newline)

;--------------------Ejercicio 11--------------------------

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
;~David

;--------------------Ejercicio 9--------------------------
(define (agregar-primo n lista)
    (if (es-primo? n)
        (append lista (list n))
        lista
    )
)

(define (generar-primos cant)
    (letrec ((iterar (lambda (n lista)
        (if (= (length lista) cant)
            lista
            (iterar (+ n 1) (agregar-primo n lista))))))
        (iterar 2 '())
    )
)

(display "Ejecución ejercicio 9: ")
(display (generar-primos 5))
(newline)
;~David

;--------------------Ejercicio 10--------------------------
;es


;--------------------Ejercicio 12--------------------------
(define (flatten lst)
  (cond ((null? lst) '())
        ((not (pair? lst)) (list lst))
        (else (append (flatten (car lst))
                      (flatten (cdr lst))))))
(flatten  '( 1 (12 23)(2 5 8)) )

; 13. Haga una función que calcule un promedio de notas a partir de una lista (Todas las notas con
;     la misma ponderación).
; Defino "suma" para utilizar en el ejercicio 18 y 13
(define (suma lst)
    (cond
        ((null? lst) 0)
        ((not (pair? lst)) (car lst))
        (else (+ (car lst) (suma (cdr lst))))
    )
)

(define (media lista)
    (letrec ((n (length lista))) (/ (suma lista) n))
)


(display "Ejecución ejercicio 13: ")
(display (media '(1 2 3)))
(newline)
;~David

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
;~David

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
;~David
(display "Ejecución ejercicio 15: ")
(display (full-comparar '(4 (5 6)) '((4 5) 6)))
(newline)


;----------------------------Ejercicio 16----------------------------
;asumo que se ingresa el valor de cada dato de la ecuacion manualmente y no una ecuacion completa
(define (discriminante a b c )
  
  (-(expt b 2)(* 4 (* a c)))
  )


;----------------------------Ejercicio 17----------------------------
;utilizo la funcion string-append para concatenar el signo a cada string de la lista
(define (agregar_exclamacion lista)
  (cond ((null? lista) '())
        (else (cons (string-append (car lista) "!") 
                    (agregar_exclamacion (cdr lista))))))



;----------------------------Ejercicio 18----------------------------
;utilizo una funcion dentro de la principal para poder iterar la lista, se comienza desde el valor de suma 0
(define (sumalistimp lista)
  (define (iterarlist lista suma)
    (cond ((null? lista) suma)
          ((odd? (car lista)) (iterarlist (cdr lista) (+ suma (car lista))))
          (else (iterarlist (cdr lista) suma))))
  (iterarlist lista 0))
; NOTA: las definiciones anidadas son, generalmente, una mala práctica, es mejor usar let para crear lambdas, eso hice a continuacion con suma-impares. ~David

(define (suma-impares lst)
    ; "suma" lo definí en el ejercicio 13
    (suma (filter (lambda(x) (not (even? x))) lst))
)

(display "Ejecución ejercicio 18: ")
(display (suma-impares '(1 2 3 4 5 6)))
(newline)
