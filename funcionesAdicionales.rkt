#lang racket
;Funcion: muestra en pantalla a usuario que se va logear
;Dominio: stack x string user x string pass.
;Recorrido: stack con usuario logeado.
;Ejemplo: (logear stack user pass)
(define (logear stack user password)
  (cambiarDato stack 3(list user password)))

;Funcion: muestra en pantalla a usuario deslogear
;Dominio: stack 
;Recorrido: stack con usuario deslogeado.
;Ejemplo: (deslogear stack)
(define (deslogear stack)
  (cambiarDato stack 3(list)))

;Funcion: ve si el nombre de usuario se encuentra en la lista de usuarios.
;Dominio: lista de usuarios x string usuario.
;Recorrido: bolean.
;Tipo recursion:
;Ejemplo: (buscadorNameUser(GetListaDeUsuarios stack)"user")
(define buscadorNameUser(lambda(lista user)
                           (cond
                              ;Hasta que llege a ser nulo
                              [( null? lista) #f]
                              ;Si no es nulo añado uno más a la cuenta del length y sigo buscando el ultimo elemento (null) de la lista
                              [(equal? (car(car lista)) user) #t]
                              [else(buscadorNameUser (cdr lista) user)])))
;Funcion: ve si el pass de user corresponde a la del user.
;Dominio: lista de usuarios x string user x pass user
;Recorrido: bolean.
;Ejemplo: (buscadorUserPassword(GetListaDeUsuarios stack)"user" "pass")
(define buscadorUserPassword(lambda(lista user password)
                           (cond
                              ;Hasta que llege a ser nulo
                              [( null? lista) #f]
                              ;Si no es nulo añado uno más a la cuenta del length y sigo buscando el ultimo elemento (null) de la lista
                              [(equal? (car(car lista)) user) (equal? (car(cdr(car lista))) password)]
                              [else(buscadorNameUser (cdr lista) user)])))


;Funcion: Nos entrega el length de una lista
;Dominio: lista x lista
;Recorrido : Entero x Entero
;Recursion Natural deja operaciones pendientes.
(define length
   (lambda (lista)
     ;Pregunto si acaso el elemento entregado acaso es una lista
     (if (list? lista)
         ;Y la recorro recurisvamente
         ( cond
            ;Hasta que llege a ser nulo
            [( null? lista) 0]
            ;Si no es nulo añado uno más a la cuenta del length y sigo buscando el ultimo elemento (null) de la lista
            [ else (+ 1 (length (cdr lista) ) ) ] )
         #f)))

;Funcion:Selector para no usar car y cdr.
;Dominio:Lista x Indice
;Recorrido: Elemento dentro de lista
(define selectorDato(lambda (elemento indice)
    (define select(lambda (elemento indice)
      (if (zero? indice)
          ;pasa esto
          (car elemento)
          ;sino
          (select (cdr elemento) (- indice 1)))))
    (if (< indice (length elemento))
    ;pasa esto
    (select elemento indice)
    ;sino
    "Indice Fuera de rango")))
;Función que cambia elemento dentro de una lista.
;Dominio : list x entero x elemento
;Recorrido : list
;Tipo recursion : natural
(define cambiarDato
  (lambda (lista posicion nuevoDato)
    ;si es vacio la lista.
    (if (null? lista)
        ;tengo mi caso base.
        lista
        ;se crea una nueva lista.
        (cons
         ;si llego a la posicion deseada devuelvo el elemento.
         (if (= 0 posicion) nuevoDato (car lista))
         ;si no sigo recorriendo la lista.
         (cambiarDato (cdr lista) (- posicion 1) nuevoDato)))))

;Función que añade una nuevo elemento a una lista.
;Dominio : lista X nuevo-elemento
;Recorrido : lista
;Tipo recursion : natural
(define añadirDato
  (lambda (lista nuevoDato)
    ;lista es nula? 
    (if (null? lista)
        ;si es devuelvo el el elemento
        (cons nuevoDato null)
        ;si no se va a ir creando una nueva lista.
        (cons
         ;primer elemento de lista
         (car lista)
         ;si no es el primero se va a ir recorriendo recursivo para añadir el elemento.
         (añadirDato (cdr lista) nuevoDato)))))

;Función la cual crea en ese instante una lista con los datos de la fecha de ese instante
;Dominio: Void
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
;Funcion: convierte en string el tiempo.
;Dominio: Lista de tiempo.
;Recorrido: string de lista de tiempo.
(define (date) (tiempo->string (get-lista-tiempo)))

;PROVIDE
(provide date)
(provide selectorDato)
(provide buscadorNameUser)
(provide buscadorUserPassword)
(provide logear)
(provide deslogear)
(provide cambiarDato)
(provide añadirDato)