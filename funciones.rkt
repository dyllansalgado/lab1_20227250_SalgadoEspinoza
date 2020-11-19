#lang racket

;Funcion para ver si el usuario se repite.
;Dominio:
;Recorrido:
;Tipo recursion:
(define buscadorNameUser(lambda(lista user)
                           (cond
                              ;Hasta que llege a ser nulo
                              [( null? lista) #f]
                              ;Si no es nulo añado uno más a la cuenta del longitud y sigo buscando el ultimo elemento (null) de la lista
                              [(equal? (car(car lista)) user) #t]
                              [else(buscadorNameUser (cdr lista) user)])))
;Funcion:
;Dominio:
;Recorrido:
(define buscadorUserPassword(lambda(lista user password)
                           (cond
                              ;Hasta que llege a ser nulo
                              [( null? lista) #f]
                              ;Si no es nulo añado uno más a la cuenta del longitud y sigo buscando el ultimo elemento (null) de la lista
                              [(equal? (car(car lista)) user) (equal? (car(cdr(car lista))) password)]
                              [else(buscadorNameUser (cdr lista) user)])))
;Funcion: logea a un usuario en el stack.
;Dominio:
;Recorrido:
(define (logear stack user password)
  (cambiarDato stack 3(list user password)))

;Funcion: deslogea a un usuario en el stack.
;Dominio:
;Recorrido:
(define (deslogear stack)
  (cambiarDato stack 3(list)))
;Usuarios2 string
;Dominio:
;Recorrido:
;Ejemplo:
(define usuarios2String
  (lambda (lista)
    (define recorrerUsuarios
      (lambda (lista string i)
        (cond
          [(null? lista) string]
          [else (recorrerUsuarios (cdr lista) (string-append string (~v i)".-" (selectorDato (car lista) 0) "Reputación : " (~v (selectorDato (car lista) 2))"\n")(+ 1 i))])))   
  (if (null? lista)
  "No hay usuarios registrados \n"
  (recorrerUsuarios lista "Usuarios Registrados : \n" 1 ))))

;Etiquetas
;Dominio:
;Recorrido:
;Ejemplo:
(define etiquetas2String
  (lambda (lista string)
    (cond
          [(null? lista) string]
          [else (etiquetas2String (cdr lista) (string-append string (car lista) ", " ))])))

;Preguntas2 string
;Dominio:
;Recorrido:
;Ejemplo:
(define preguntas2String
  (lambda (lista listaR)
    (define recorrerPreguntas
      (lambda (lista string i)
        (cond
          [(null? lista) string]
          [else (recorrerPreguntas (cdr lista)
          (string-append string (~v i)".-" "ID : " (~v (selectorDato (car lista) 0)) " Pregunta : " (selectorDato (car lista) 1) " Autor : " (selectorDato (car lista) 2)  (etiquetas2String (selectorDato (car lista) 3) " Etiquetas : ") " Fecha :" (selectorDato (car lista) 4) " Reward : "(~v (selectorDato (car lista) 5))"\n"
                          (respuestas2String listaR (selectorDato (car lista) 0) ))(+ 1 i))])))   
  (if (null? lista)
  "No hay preguntas realizadas \n"
  (recorrerPreguntas lista "Preguntas realizadas : \n" 1 ))))
;Recorrido : lista de respuestas idPregunta
;Dominio : 
;Ejemplo : (respuestas2String (GetListaDeRespuestas stackoverflow) 1)
;Tipo recursion:
(define respuestas2String
  (lambda(lista idPregunta )
    (define recorrerRespuestas
      (lambda (lista string i)
        (cond
          [( null? lista) string]
          [else (recorrerRespuestas (cdr lista) (string-append string (~v i)  ".- ID : "(~v (selectorDato (car lista)0)) " Autor :" (selectorDato (car lista)1) " Respuesta :" (selectorDato (car lista)3) " Fecha : " (selectorDato (car lista)2) (etiquetas2String (selectorDato (car lista) 4) " Etiquetas : ") ) (+ i 1)) ])

        ))
    (cond
      ;Hasta que llege a ser nulo
      [(null? lista) "alo"]
      ;Si no es nulo sigo buscando el ultimo elemento (null) de la lista
      [(equal? (car(car lista)) idPregunta)  (recorrerRespuestas (cdr (car lista)) (string-append "Respuestas de la pregunta de ID : " (~v (car(car lista))) "\n") 1)  ]
      [else(respuestas2String (cdr lista) idPregunta)])))
;Funcion : calcular longitud
;Dominio: lista x lista
;Recorrido : entero x entero
;Tipo recursion : natural porque deja estados pendientes. 
(define length
   (lambda (lista)
     ;Pregunto si acaso el elemento entregado acaso es una lista
     (if (list? lista)
         ;Y la recorro recurisvamente
         (cond
            ;Hasta que llege a ser nulo
            [( null? lista) 0]
            ;Si no es nulo añado uno más a la cuenta del longitud y sigo buscando el ultimo elemento (null) de la lista
            [ else (+ 1 (length (cdr lista) ) ) ] )
         #f)))

;Selector de Zonas segun indice sinonimo de list-ref
;Dominio: lista y el indice
;Recorrido: elemento de una lista
(define selectorDato(lambda (elemento indice)
    (define select(lambda (elemento indice)
      (if (zero? indice)
          ;True Case
          (car elemento)
          ;Else
          (select (cdr elemento) (- indice 1)))))
    (if (< indice (length elemento))
    ;True Case
    (select elemento indice)
    ;Else
    "el indice no se encuentra")))
;Función : cambia elemento de una lista.
;Dominio : lista x entero x elemento
;Recorrido : lista
;Tipo recursion : natural.
(define cambiarDato
  (lambda (lista posicion nuevoDato)
    ;Pregunto si la lista es nula 
    ;pregunto si ya llege a mi posición deseada
    ;en caso de serlo devuelvo el elemento
    ;En caso contr
    (if (null? lista)
        ;Caso base , cuando ya termine de de recorrer toda la lista
        lista
        ;Else recursivo
        ;CREO UNA NUEVA LISTA reconstruyendola toda
        (cons
         ;pregunto si ya llege a mi posición deseada
         ;en caso de serlo devuelvo el elemento
         (if (= 0 posicion) nuevoDato (car lista))
         ;En caso contrario sigo recorriendo la lista recursivamente 
         (cambiarDato (cdr lista) (- posicion 1) nuevoDato)))))

;Función que añade una nuevo elemento a una lista
;Dominio : lista X nuevo-elemento
;Recorrido : lista
;Tipo recursion : natural
(define añadirDato
  (lambda (lista nuevoDato)
    ;Pregunto si la lista es nula 
    (if (null? lista)
        ;en caso de serlo devuelvo el elemento
        (cons nuevoDato null)
        ;Else recursivo
        ;CREO UNA NUEVA LISTA reconstruyendola toda
        (cons
         ;El primer elemento de la imagen
         (car lista)
         ;En caso contrario sigo recorriendo la lista recursivamente 
         (añadirDato (cdr lista) nuevoDato)))))

;Función la cual crea en ese instante una lista con los datos de la fecha de ese instante
;Dominio:Void
;Recorrido: Lista
(define get-lista-tiempo
  (lambda ()
    (list
     (date-second (seconds->date (current-seconds)))
     (date-minute (seconds->date (current-seconds)))
     (date-hour (seconds->date (current-seconds)))
     (date-day (seconds->date (current-seconds)))
     (date-month (seconds->date (current-seconds)))
     (date-year (seconds->date (current-seconds))))))
;(define lista (get-lista-tiempo))

;Funcion traductora a string para que se más comprensible para el ususario
;Dominio: Lista
;Recorrido : String
(define tiempo->string
  (lambda (listaTiempo)
    (string-append (~v (selectorDato listaTiempo 2))":"
                   (~v (selectorDato listaTiempo 1))":"
                   (~v (selectorDato listaTiempo 0))" "
                   (~v (selectorDato listaTiempo 3))"/"
                   (~v (selectorDato listaTiempo 4))"/"
                   (~v (selectorDato listaTiempo 5))"\n")))
;
(define (date) (tiempo->string (get-lista-tiempo)))

(provide date)
(provide selectorDato)
(provide logear)
(provide deslogear)
(provide buscadorNameUser)
(provide buscadorUserPassword)
(provide preguntas2String)
(provide etiquetas2String)
(provide usuarios2String)
(provide respuestas2String)
;Modificadores
(provide cambiarDato)
(provide añadirDato)
