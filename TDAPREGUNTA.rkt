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

;Pertenencia
;;Funciones de pertenencia para register.
;;Preguntamos si la lista esta vacia, si es asi devuelve true.
(define (listaregister? listaregister)
  (if (null? listaregister)
      #t
      ;;preguntamos si listaregister es una lista ("" "")
      (if (list? listaregister)
          ;;preguntamos si es un articulo 
           (if (articulo? (car listaregister) )
               (listaregister? (cdr listaregister))
               #f
           )
           #f
      )
  )
)
;;definimos que es un articulo
(define (articulo? art)
  ;;si articulo es una lista
  (if (list? art)
     ;;tiene que ser de orden 2 (user, pass)
    (if (= 2 (length art))
       ;;preguntamos si (user es string y si pass es string)
      (and (string? (car art)) (string? (cadr art)))
      ;;si no se cumple devuelve False
           #f)
      ;;si no se cumple devuelve false.
      #f)
)

;Selectores
;;Funciones de selectores para register.
(define articulo->getuser (lambda (art)
              ;;pregunto si art es un articulo              
             (if (articulo? art)
                 ;;devuelve el user
                 (car art)
                 (raise "No es un artículo")
                 )
       )
)
(define articulo->getpass (lambda (art)
              ;;pregunto si art es un articulo
             (if (articulo? art)
                 ;;devuelve la pass
                 (cdr art)
                 (raise "No es un artículo")
                 )
       )
)

;Modificadores
;;funcion que crea un nuevo usuario a partir de otro usuario.
;;Le doy un nuevo nombre y la pass.
(define (articulo->setuser art nvoNombre)
        (if (articulo? art)
        (articulo nvoNombre (articulo->getpass art)
        )
        (raise "No es un artículo")
         )
)
;;Funcion para agregar usuarios a lista de usuarios.

;; Ejemplo:(adduser(adduser(register)"holi2" "soyclave2222")"hola3" "s")
(define (adduser listauser user pass)
  ;preguntamos si es una listaregister
  (if (listaregister? listauser)
    ;;cons agregamos elemento a la izquierda ("user" "pas") y listauser es la lista.
    (cons (articulo user pass) listauser)
    ;;si no cumple se muestra en pantalla.
    (raise "No es una lista de compras")
 )
)







