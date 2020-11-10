#lang racket
;Que debo ir creando para la opcion register.

;1)Primero debo crear una lista vacia que solicitara los datos de usuario y contraseña.
;2)Asignar la lista que sera tipo ( user, pass) a otra lista contenedora llama lista de usuarios.
;3)Luego debo ver si el nombre de usuario se encuentra en la lista de usuarios (con recursion)
;4)Si se encuentra debo mostrar por pantalla que el nombre de usuario no se encuentra disponible y que debe volver a registrarse.

;Que debo ir creando para la opcion login.

;1)Si el usuario pudo completar el registro,(nombre correcto y pass), debo pedir que ingrese buscando los datos dentro de la
;funcion lista de usuarios.
;2)Debo ir recorriendo la lista hasta encontrar el (nombre y pass), si no se encuentra es porque se equivoco de user o pass.


;Constructores
;descripción: Permite crear un registro
;dom: string x string
;rec: lista
;ejemplo de uso : (register "user1" "pass1")
(define (register user pass)
  (if (and (string? user) (string? pass))
      (list user pass)
      (raise "No es un registro de usuario valido")
  )
)
;Constructor para etiquetas
;descripción: Permite crear una lista de etiquetas
;dom: string 
;rec: lista
;ejemplo de uso : (etiquetas "racket" "scheme")
(define (etiquetas etiquetas)
  (if (or (string? etiquetas)(null? etiquetas))
      (list etiquetas)
      (raise "No son etiquetas") 
   )
)
;Constructor para preguntas
;descripción: Permite crear una pregunta
;dom: string x lista de string
;rec: lista
;ejemplo de uso : 
(define (ask ask etiquetas)
  (if (and(string? ask)(list? etiquetas))
      (list ask)
      (raise "No es una pregunta")
  )
)

;Constructor para respuestas
;(define (answer)
  ;)

;CONSTRUCTOR USUARIO
(define (preguntas fecha ask answer)
    (if (and (string? fecha) (list? ask) (list? answer))
             (list fecha ask answer)
             (raise "No cumple los parametros")       
             )
 )

;Pertenencia
;;Funciones de pertenencia para register.
;;Preguntamos si la lista esta vacia, si es asi devuelve true.
(define (register? register)
  (if (null? register)
      #t
      ;;preguntamos si listaregister es una lista ("" "")
      (if (list? register)
          ;;preguntamos si es un articulo 
           (if (register? (car register) )
               (register? (cdr register))
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
        (register nvoNombre (articulo->getpass art)
        )
        (raise "No es un artículo")
         )
)
;;Funcion para agregar usuarios a lista de usuarios.

;; Ejemplo:(adduser(adduser(register)"holi2" "soyclave2222")"hola3" "s")
(define (adduser listauser user pass)
  ;preguntamos si es una listaregister
  (if (register? listauser)
    ;;cons agregamos elemento a la izquierda ("user" "pas") y listauser es la lista.
    (cons (register user pass) listauser)
    ;;si no cumple se muestra en pantalla.
    (raise "No es una lista de compras")
 )
)







