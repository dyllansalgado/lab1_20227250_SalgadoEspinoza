#lang racket
;Que debo ir creando para la opcion register.

;1)Primero debo crear una lista vacia que solicitara los datos de usuario y contraseña.
;2)Asignar la lista que sera tipo ( user, pass) a otra lista contenedora llama lista de usuarios.
;3)Luego debo ver si el nombre de usuario se encuentra en la lista de usuarios (con recursion)
;4)Si se encuentra debo mostrar por pantalla que el nombre de usuario no se encuentra y que debe volver a registrarse.

;Que debo ir creando para la opcion login.

;1)Si el usuario pudo completar el registro,(nombre correcto y pass), debo pedir que ingrese buscando los datos dentro de la
;funcion lista de usuarios.
;2)Debo ir recorriendo la lista hasta encontrar el (nombre y pass), si no se encuentra es porque se equivoco de user o pass.


;Constructores
;;Creamos una lista vacia que contendra a los registros.
(define (register )
  (list )
)
;;Revisa que sea un articulo que cumpla con 2 strings.
;;ej;(articulo "hola" "hola") duelve ("hola" "hola").
;;si se pone (articulo "hola" 2) duelve parametros invalidos
(define (articulo user pass)
  (if(and (string? user) (string? pass))
     (list user pass)
     (raise "parametros invalidos")
     )
  )

;Creamos una lista vacia que contendra a los usuarios.
(define (usuariosvacio )
  (list )
)
;Creamos una lista vacia que tendra las preguntas en el foro.
(define (foropreguntasvacio )
  (list )
)






