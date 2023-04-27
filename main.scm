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

;--------------------Ejercicio 9--------------------------
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

;--------------------Ejercicio 11--------------------------

;--------------------Ejercicio 12--------------------------
(define (flatten lst)
  (cond ((null? lst) '())
        ((not (pair? lst)) (list lst))
        (else (append (flatten (car lst))
                      (flatten (cdr lst))))))
(flatten  '( 1 (12 23)(2 5 8)) )
;13