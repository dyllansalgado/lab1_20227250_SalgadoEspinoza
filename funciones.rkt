#lang racket
;FUNCION : calcular longitud o length , su implmentación es solo para saber como funcionaba 
;Dominio: lista x lista
;Recorrido : Entero x Entero
;Recursion Natural  , ya que deja espacios pendientes 
(define longitud
   (lambda (lista)
     ;Pregunto si acaso el elemento entregado acaso es una lista
     (if (list? lista)
         ;Y la recorro recurisvamente
         ( cond
            ;Hasta que llege a ser nulo
            [( null? lista) 0]
            ;Si no es nulo añado uno más a la cuenta del longitud y sigo buscando el ultimo elemento (null) de la lista
            [ else (+ 1 (longitud (cdr lista) ) ) ] )
         #f)))

;Selector de Zonas segun indice sinonimo de list-ref
;Dominio:Lista x Indice
;Recorrido: Elemento dentro de lista
(define selectorDato(lambda (elemento indice)
    (define actuador(lambda (elemento indice)
      (if (zero? indice)
          ;True Case
          (car elemento)
          ;Else
          (actuador (cdr elemento) (- indice 1)))))
    (if (< indice (longitud elemento))
    ;True Case
    (actuador elemento indice)
    ;Else
    "Indice Fuera de rango")))
;Función que cambia elemento dentro de una lista
;DOMINIO : LISTA x ENTERO x ELEMENTO
;RECORRIDO : LISTA
;RECURSIÓN : NATURAL
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
;DOMINIO : lista X nuevo-elemento
;RECORRIDO : lista
;RECURSIÓN : NATURAL
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
;Función que añade al inicio un elemento
;Dominio : lista x elemento
;Recorrido : Lista
(define añadir-al-inicio
  (lambda (lista nuevoDato)
    (reverse(añadirDato (reverse lista) nuevoDato))))



(provide selectorDato)
;Modificadores
(provide cambiarDato)
(provide añadir-al-inicio)
(provide añadirDato)
