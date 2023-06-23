#lang plai

;; BNF
;;<F1WAE> ::-<number>
;;        | {+ <F1WAE> <F1WAE>}
;;        | {- <F1WAE> <F1WAE>}      
;;        | {with {<id> <F1WAE>} <F1WAE>}
;;        | {<id> <F1WAE>}      

;; AST para F1WAE: First-Orden Functions With Aritmetic Expression
(define-type F1WAE 
  [num (n number?)] 
  [add (l F1WAE?) (r F1WAE?)] 
  [sub (l F1WAE?) (r F1WAE?)] 
  [with (name symbol?) (value F1WAE?) (body F1WAE?)] 
  [id (name symbol?)]
  [app (fun-name symbol?) (arg F1WAE?)]
  [structure (struct-name symbol?) (index symbol?)])


;; AST para una funcion
(define-type FunDef
  [fundef (fun-name symbol?) (arg-name symbol?) (body F1WAE?)])

;; AST para environments
(define-type Env
  [mtEnv]
  [aEnv (name symbol?) (value number?)
        (rest Env?)])

;;AST structs
;;(define-type Struct
  ;;[str (name symbol?) (fields (listof (cons symbol? number?)))])

;; parse: Program -> AST (F1WAE)
(define (parse program)
  (cond
    [(number? program) (num program)]
    [(symbol? program) (id program)]
    [(list? program)
     (case (first program)
       [(+) (add (parse(second program))
                 (parse(third program)))]
       [(-) (sub (parse(second program))
                 (parse(third program)))]
     
       [(with) (with (first (second program))
                     (parse (second (second program)))
                     (parse (third program)))]
       [(-s) (structure (second program)
                 (third program))]
       [else (app (first program) (parse (second program)))])]))
 



;; lookup-fundef : symbol (id) x Lista de FunDefs -> FunDef
(define (lookup-function fun-name fundefs)
  (cond
    [(empty? fundefs) (error fun-name "function not found")]
    [else (if (symbol=? fun-name (fundef-fun-name (car fundefs)))
            (car fundefs)
            (lookup-function fun-name (rest fundefs)))]))






;; lookup-env: symbol (nombre de variable) x Env → F1WAE (un valor del lenguaje)
(define (lookup-env name env)
  (type-case Env env
             ;; si es vacio, retornamos error
             [mtEnv () (error 'lookup "free identifier")]
             ;; sino, comparamos en el valor buscado
             [aEnv (bound-name value rest)
                   (if (symbol=? bound-name name)
                       value
                       (lookup-env name rest))]))





;; lookup-struct-dict
(define (lookup-struct-dict symbols-dict symbol-name)
  (if (dict-ref symbols-dict symbol-name #f)
    (dict-ref symbols-dict symbol-name)
    (error symbol-name "key not found")))

;; diccionario de structs
(define structs
  '((Scarlet . ((edad . 25) (salario . 2000000)))
    (Persona . ((nombre . "Paul") (apellido . "Leger") (comida-favorita . "empanada")))
    (UCN . ((años-acreditacion . 6) (mejor-carrera . "ICCI")))
))

;; Interprete
;; interp : AST x Lista de fundef -> number 
(define (interp expr fundefs structs env)
  (type-case F1WAE expr
             [num (n) n]
             [add (l r) (+ (interp l fundefs structs env) (interp r fundefs structs env))]
             [sub (l r) (- (interp l fundefs structs env) (interp r fundefs structs env))]
    
             [with (id named-expr body)
                   (interp body fundefs structs
                           (aEnv id
                                 (interp named-expr fundefs structs env)
                                 env))]

    
             [id (name) (lookup-env name env)]
             [app (fun-name arg-expr)
                  (let ([fun (lookup-function fun-name fundefs)])
                    (interp (fundef-body fun) fundefs structs
                            (aEnv (fundef-arg-name fun)
                                  (interp arg-expr fundefs structs env)
                                  (mtEnv))))]
             [structure (struct-name key)
                   (lookup-struct-dict (lookup-struct-dict structs struct-name) key)]
                   ))


;; Una función para simplificar el uso, pero usando funciones
;; ejecutar: Program x Lista de funDef -> number
(define (ejecutar program [list-funs '()] [list-structs '()]) 
  (interp (parse program) list-funs list-structs (mtEnv)))

; TEST STRUCT
(printf "~nTEST STRUCT~n")
(test (parse '{+ {with {x {-s UCN años-acreditacion}} {+ x 4}} {-s Scarlet edad}}) (add (with 'x (structure 'UCN 'años-acreditacion) (add (id 'x) (num 4))) (structure 'Scarlet 'edad)))
(test (parse '{+ 4 {-s Scarlet edad}}) (add (num 4) (structure 'Scarlet 'edad)))
(test (ejecutar '{+ 4 {-s Scarlet edad}} '() structs) 29)
(test (ejecutar '{- {-s UCN años-acreditacion} 1} '() structs) 5)
(test/exn (ejecutar '{-s Scarlet apellido} '() structs) "key not found")
(test/exn (ejecutar '{-s ULS razones-para-preferirla} '() structs) "key not found")
(test (ejecutar '{+ {-s UCN años-acreditacion} {-s Scarlet edad}} '() structs) 31)
(test (ejecutar '{double {-s Scarlet salario}} (list (fundef 'double 'n (add (id 'n) (id 'n)))) structs) 4000000)
(test (ejecutar '{-s UCN mejor-carrera} '() structs) "ICCI")
(test (ejecutar '{-s Persona comida-favorita} '() structs) "empanada")


; TEST PARSER
(printf "~nTEST PARA PARSER~n")
(test (parse '3) (num 3))
(test (parse '{+ 1 2}) (add (num 1) (num 2)))
(test (parse '{+ {- 2 1} 3}) (add (sub (num 2) (num 1)) (num 3)))
;; withs
(test (parse '{with {x 1} 1}) (with 'x (num 1) (num 1)))
(test (parse '{with {x 2} x}) (with 'x (num 2) (id 'x)))
(test (parse '{with {x 1} {+ x 1}}) (with 'x (num 1) (add (id 'x) (num 1))))
(test (parse '{with {x {+ 3 1}} x}) (with 'x (add (num 3) (num 1)) (id 'x)))
(test (parse '{+ {with {x 4} x} 3}) (add (with 'x (num 4) (id 'x)) (num 3)))

(printf "~nTEST PARA INTERPRETES~n")
; TEST INTERPRETE
(test (ejecutar '3) 3)
(test (ejecutar '{+ 1 2}) 3)
(test (ejecutar '{+ {- 2 1} 3}) 4)
;;withs
(test (ejecutar '{with {x 1} 1}) 1)
(test (ejecutar '{with {x 2} x}) 2)
(test (ejecutar '{with {x 2} {+ x 1}}) 3)
(test (ejecutar '{with {x 5} {+ x {with {x 3} 10}}}) 15)
(test (ejecutar '{with {x 5} {+ x {with {x 3} x}}}) 8)
(test (ejecutar '{with {x 5} {+ x {with {y 3} x}}}) 10)
(test (ejecutar '{with {x 5} {with {y x} y}}) 5)
(test/exn (ejecutar '{with {x x} x}) "free identifier")
;; funs
(test (ejecutar '{double 5} (list (fundef 'double 'n (add (id 'n) (id 'n))))) 10)
(test (ejecutar '{add1 {double 5}} (list (fundef 'double 'n (add (id 'n) (id 'n))) (fundef 'add1 'num (add (num 1) (id 'num))))) 11)
(test/exn (ejecutar '{triple 5} (list (fundef 'double 'n (add (id 'n) (id 'n))))) "function not found")
(test/exn (ejecutar '{with {x 3} {f 2}} (list (fundef 'f 'n (parse '{+ n x})))) "free identifier") 
