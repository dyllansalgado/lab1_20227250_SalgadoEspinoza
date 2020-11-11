#lang racket
(require "funciones.rkt")
;EJEMPLO 2
;sin usuarios.
(define stackoverflow2
  (list
   ;0 tiene todos los usuarios y contraseñas
   (list  )
   )
)
;EJemplo de stack 
;EJEMPLO 1
;con 1 usuario
;TDA stackoverflow
(define stackoverflow
  (list
  ;0 tiene todos los usuarios y contraseñas
  ;TDA usuario contraseña
  ;LISTA
  ;TDA base de datos de usuarios y contraseñas
  ;LISTA X LISTAS
  (list (list "juan01" "clave123"))
  ;TDA PREGUNTA 1/0 es respondida o no.
  (list (list 1 "pregunta" "juan01" (list "etiquetas" "C" "universidad") "autor" "10/12/2020" "estado" "1/0" ))
  ;TDA Respuestas
  (list (list 1 (list "diego02" "12/12/2020"  "respuesta" (list "etiquetas" "c"))))
  )
)
;Pertenencia de stackoverflow.
;Ejemplo:(esStackoverflow? stackoverflow)
(define (esStackoverflow? stackoverflow)
  (if (list? stackoverflow)
      (and (list? (selectorIndice stackoverflow 0))
          (list? (selectorIndice stackoverflow 1))
          (list? (selectorIndice stackoverflow 2)))
  #f
  )
)

;Funcion register
(define register(lambda(stackoverflow user pass)
                  (if (and (esStackoverflow? stackoverflow)(string? user)(string? pass))
                      (añadir-elemento(selectorIndice stackoverflow 0)(list user pass))
                      (raise "No corresponden a usuario y pass")
                  )
               )
 )

;Funcion para ver si el usuario se repite.

