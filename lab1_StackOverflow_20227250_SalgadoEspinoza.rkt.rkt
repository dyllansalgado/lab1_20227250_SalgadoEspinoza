#lang racket
(require "funcionesAdicionales.rkt")
(require "TDAFecha.rkt")
;CONSTRUCTOR
;Constructor stackoverflow
;Dominio: lista de registros x lista de ask , lista de answer, lista sesion.
;Recorrido: lista de listas, que contendra a los registros, ask, answer y sesion.
;Ejemplo:(stackoverflow (list) (list) (list) (list))
(define (stackoverflow ListRegister ListAsk ListAnswer ListSesion)(list ListRegister ListAsk ListAnswer ListSesion))

;PERTENENCIA
;Pertenencia de stackoverflow.
;Dominio: stack.
;Recorrido: bolean.
;Ejemplo:(esStackoverflow? stackoverflow)
(define (esStackoverflow? stackoverflow)
  (if (list? stackoverflow)
      (and (list? (selectorDato stackoverflow 0))
          (list? (selectorDato stackoverflow 1))
          (list? (selectorDato stackoverflow 2)))
  #f
  )
)
;Funcion: comprobar si existe un usuario.
;Dominio: lista registros en stack.
;Recorrido: bolean.
;ejemplo : (existNameUser? stackoverflow "user")
(define existNameUser?(lambda(stackoverflow user)
                        (if(null? (selectorDato stackoverflow 0))
                           #f
                           (buscadorNameUser (selectorDato stackoverflow 0) user))))

;Funcion: sirve para ver si la pregunta corresponde a un usuario.
;Dominio: lista de preguntas del stack x user.
;Recorrido: bolean.
;Tipo recursion: De cola.
;Ejemplo: (preguntaCorresponde? (GetListaDePreguntas stackoverflow) id "user")
(define preguntaCorresponde?
  (lambda (lista id user)
    (cond
      ;Hasta que llege a ser nulo
      [( null? lista) #f]
      ;Si no es nulo sigo buscando el ultimo elemento (null) de la lista
      [(equal? (car(car lista)) id) (equal? (selectorDato (car lista) 2) user) ]
      [else(preguntaCorresponde? (cdr lista) id user)])))

;Funcion: sirve para ver si la pregunta existe por su id.
;Dominio: lista de preguntas en stack x id.
;Recorrido: bolean.
;Tipo recursion: De cola.
;Ejemplo:(existePreguntaID? (GetListaDePreguntas stackoverflow) id )
(define existePreguntaID?(lambda(lista id)
                            (cond
                              ;Hasta que llege a ser nulo
                              [( null? lista) #f]
                              ;Si no es nulo sigo buscando el ultimo elemento (null) de la lista
                              [(equal? (car(car lista)) id) #t ]
                              [else(existePreguntaID? (cdr lista) id)])))

;Funcion: nos permite saber si la respuesta existe a la pregunta por su id.
;Dominio: lista de respuestas x idPregunta  x idRespuesta.
;Recorrido: bolean
;Tipo recursion: De cola.
;Ejemplo: (existeRespuestaID? (GetListaDeRespuestas stackoverflow) idPregunta idRespuesta)
(define existeRespuestaID?(lambda(lista idPregunta idRespuesta)
                            (cond
                              ;Hasta que llege a ser nulo
                              [( null? lista) #f]
                              ;Si no es nulo sigo buscando el ultimo elemento (null) de la lista
                              [(equal? (car(car lista)) idPregunta) (existePreguntaID? (cdr(car lista)) idRespuesta) ]
                              [else(existeRespuestaID? (cdr lista) idPregunta idRespuesta)])))
;SELECTORES
;Fucnion: Selectores para el stack.
;Dominio: stack y posicion.
;Recorrido: Lista con datos que seleccionamos.
;Ejemplo: (GetListaDeUsuarios stackoverflow)
(define (GetListaDeUsuarios stack) (selectorDato stack 0))
;Ejemplo:(GetListaDePreguntas stackoverflow)
(define (GetListaDePreguntas stack) (selectorDato stack 1))
;Ejemplo:(GetListaDeRespuestas stackoverflow)
(define (GetListaDeRespuestas stack) (selectorDato stack 2))
;Ejemplo:(GetActiveUsuario stackoverflow)
(define (GetActiveUsuario stack) (selectorDato stack 3))
;Ejemplo:(GetUsuarioLogeado stackoverflow)
(define (GetUsuarioLogeado stack) (selectorDato (selectorDato stack 3) 0))

;Funcion: Obtener cuanto reward da cierta pregunta dado el id de esta.
;Dominio: lista pregunta x id pregunta
;Recorrido: reward 
;Ejemplo:(getRewardPregunta (GetListaDePreguntas stackoverflow) 1)
(define getRewardPregunta (lambda (lista idPregunta)
                            (selectorDato (selectorDato lista (getIndicePreguntaID lista idPregunta)) 5)))

;Funcion: Obtener el usuario de una respuesta
;Entrada: lista de respuestas en el stack x id de la pregunta x id de respuesta.
;Recorrido: Usuario que realiza pregunta, si no se encuentra entrega #f
;Tipo de recursion: natural.
;Ejemplo: (getUsuarioAnswer (GetListaDeRespuestas stackoverflow) 1 1)
(define getUsuarioAnswer
(lambda(lista idPregunta idRespuesta)
  (define buscarUsuario
    (lambda (lista idRespuesta)
      (cond
        ;Hasta que llege a ser nulo
        [( null? lista) #f]
        ;Si no es nulo sigo buscando el ultimo elemento (null) de la lista
        [(equal? (car(car lista)) idRespuesta) (selectorDato (car lista) 1) ]
        [else(buscarUsuario (cdr lista) idRespuesta)])))
  (cond
    ;Hasta que llege a ser nulo
    [( null? lista) #f]
    ;Si no es nulo sigo buscando el ultimo elemento (null) de la lista
    [(equal? (car(car lista)) idPregunta) (buscarUsuario (cdr(car lista)) idRespuesta) ]
    [else(getUsuarioAnswer (cdr lista) idPregunta idRespuesta)])))

;Funcion: Obtiene el indice del usuario entregado.
;Entrada: Se le entrega el nombre del usuario.
;Salida: Indice de la lista que corresponde dicho nombre de usuario.
;Tipo de recursion: 
;Ejemplo:(getIndiceUsuario stackoverflow "usuario")
(define getIndiceUsuario
  (lambda (stack usuario)
    (define buscarIndice
      (lambda (lista usuario i)
        (cond
          ;Hasta que llege a ser nulo
          [( null? lista) -1]
          ;Si no es nulo sigo buscando el ultimo elemento (null) de la lista
          [(equal? (car(car lista)) usuario) i]
          [else (buscarIndice (cdr lista) usuario (+ i 1))])))
    (buscarIndice (GetListaDeUsuarios stack) usuario 0)))

;Funcion: obtener indice de pregunta en una lista de respuestas.
;Entrada: lista de respuestas en stack x id repuesta.
;Salida: devuelve el indice de la respuesta y si no la encuentra devuelve un -1.
;EJEMPLO : (getIndicePreguntaID (GetListaDeRespuestas stackoverflow) 1)
(define getIndicePreguntaID (lambda(lista id)
                (define buscar (lambda(lista id i)
                            (cond
                              ;Hasta que llege a ser nulo
                              [( null? lista) #f]
                              ;Si no es nulo sigo buscando el ultimo elemento (null) de la lista
                              [(equal? (car(car lista)) id) i ]
                              ;Caso recursivo
                              [else(buscar (cdr lista) id (+ i 1))])))
               (buscar lista id 0)))



;MODIFICADORES:

;Funcion: asigna el valor del reward a la pregunta.
;Entrada: lista de preguntas del stack x id x reward x stack x indice.
;Salida: pregunta con reward asignado.
;Ejemplo:(asignarRewardPreguntaID (GetListaDePreguntas stackoverflow) id reward stackoverflow indicepregunta)
(define asignarRewardPreguntaID(lambda (lista id reward stack i)
                            (cond
                              ;Hasta que llege a ser nulo
                              [( null? lista) #f]
                              ;Si no es nulo sigo buscando el ultimo elemento (null) de la lista
                              [(equal? (car(car lista)) id) (cambiarDato stack 1
                              (cambiarDato (GetListaDePreguntas stack) i (asignarReward (car lista) reward)) ) ]
                              [else(asignarRewardPreguntaID (cdr lista) id reward stack (+ i 1))])
                            ))
;Funcion: Asigno el reward a la pregunta.
;Dominio: pregunta x reward.
;Recorrido: reward ingresado.
(define asignarReward (lambda (pregunta reward)
                        (cambiarDato pregunta 5 reward)))

;Funcion: Asigna el reward que se ha ingresado a un usuario.
;Dominio: stack X usuario X reward.
;Recorrido: Usuario con reward otorgado.
;Ejemplo:(asignarRewardUsuario stackoverflow "user" 100)
(define asignarRewardUsuario (lambda (stack usuario reward)
                               (cambiarDato stack 0 (cambiarDato (GetListaDeUsuarios stack) (getIndiceUsuario stack usuario) (cambiarDato (selectorDato (GetListaDeUsuarios stack) (getIndiceUsuario stack usuario)) 2 reward ))) ))


;Funcion: Responder pregunta segun el id.
;Dominio: stack x listarespuestas x id x answer x date x etiquetas.
;Recorrido: stack con pregunta respondida.
(define responderPreguntaID (lambda (stack listaRespuestas id respuesta date etiquetas)
                               ;caso de que la lista de respeustas a la pregunta este vacía o no exista la respuesta aún
                               (if (or (null? listaRespuestas) (equal? (getIndicePreguntaID listaRespuestas id) #f))
                                   (cambiarDato stack 2
                                    (añadirDato listaRespuestas (list id (list 1 (GetUsuarioLogeado stack) date respuesta etiquetas))))
                                   ;Caso de que si hayan respuestas
                                   (cambiarDato stack 2
                                   (cambiarDato listaRespuestas (getIndicePreguntaID listaRespuestas id)
                                    (añadirDato (selectorDato listaRespuestas (getIndicePreguntaID listaRespuestas id)) (list (length (selectorDato listaRespuestas (getIndicePreguntaID listaRespuestas id))) (GetUsuarioLogeado stack) date respuesta etiquetas)))))))

;Funcion: para cuando un usuario no tenga reputacion y de recompensa no pueda ser entregada.
;Dominio: integer.
;Recorrido: integer.
(define esNegativo(lambda(numero)
                    (if (> 0 numero)
                        0
                        numero)))
;REGISTER
;Funcion: registra a un usuario en el stack.
;Dominio: stack x string x string.
;Recorrido: stack con el registro completado.
;Ejemplo: (register stackoverflow "user" "pass")
(define register(lambda(stackoverflow user pass)
                  (if (and (esStackoverflow? stackoverflow)(string? user)(string? pass))
                      (if (existNameUser? stackoverflow user)
                      "Ya existe este nombre de usuario\n"
                      (cambiarDato stackoverflow 0 (añadirDato(selectorDato stackoverflow 0)(list user pass 0))))
                      "No corresponden a usuario y pass\n"
                  )
               )
 )
         
;LOGIN 
;Funcion : permite al registro realizado poder ingresar.
;Dominio: stack x string x string x funcion
;Salida: stack con funcion realizada
;Tipo de recursion: Se utiliza recursion natural en funcion buscadorNameUser.
;Ejemplo : (login stackoverflow "user" "pass" ask)
(define login (lambda (stack username password operation)
                (if (and (esStackoverflow? stack) (string? username) (string? password))
                    (if (buscadorNameUser (selectorDato stack 0) username)
                        (if (buscadorUserPassword (selectorDato stack 0) username password)
                           (cond
                             ;ask
                             [(equal? operation ask)
                              (ask (logear stack username password))]
                             ;reward
                             [(equal? operation reward)
                              (reward (logear stack username password))]
                             ;answer
                             [(equal? operation answer)
                              (answer (logear stack username password))]
                             ;acept
                             [(equal? operation accept)
                              (accept (logear stack username password))]
                             [else
                              (display "Comando Invalido\n")])
                           operation
                         )
                        operation
                        )
                    operation
                    )))
;ASK
;Funcion: permite a un usuario con sesión iniciada en la plataforma realizar una nueva pregunta.
;Dominio: stack
;Recorrido :list ->stack X date X string X string list
;Ejemplo :(((login stackoverflow "user" "pass" ask)(date))"question?" "C#")
;Ejemplo :(((login stackoverflow "user" "pass" ask)(fecha 1 2 3))"question?" "C#")
(define (ask stack)(lambda (date)
               (lambda (question .labels)
                 (if (not (null? (GetActiveUsuario stack)))
                 (deslogear (cambiarDato stack 1 (añadirDato
                  (GetListaDePreguntas stack)
                  (list (+ (length (GetListaDePreguntas stack)) 1)
                  question (GetUsuarioLogeado stack)
                  (list .labels) date 0
                  ) )))
                 "No hay usario logeado \n"))))

;REWARD
;Funcion: permite a un usuario con sesión iniciada en la plataforma ofrecer una recompensa para una determinada pregunta.
;Dominio: stack
;Recorrido: integer(id de pregunta) x integer (recompensa)
;Ejemplo:(((login stackoverflow "user" "pass" reward) 1) 300)
(define (reward stack)(lambda(id)
                 (lambda (reward)
                   (if (not (null? (GetActiveUsuario stack)))
                   (if (existePreguntaID? (GetListaDePreguntas stack) id)
                       (if (preguntaCorresponde? (GetListaDePreguntas stack) id (GetUsuarioLogeado stack))
                       (deslogear (asignarRewardPreguntaID (GetListaDePreguntas stack) id reward stack 0))
                        "El usuario entregado, no ha formulado dicha pregunta" )
                       "No existen preguntas")
                   "No hay usario logeado \n"))))

;ANSWER
;Funcion: permite a un usuario con sesión iniciada en la plataforma responder una pregunta.
;Dominio: stack
;Recorrido: stack con respuesta ingresada.
;Ejemplo : ((((login stackoverflow "user" "pass" answer) (date)) 1) "mi respuesta" "C")
(define (answer stack)(lambda (date)
                        (lambda(id)
                          (lambda (answer .labels)
                            (if (existePreguntaID? (GetListaDePreguntas stack) id)
                                (deslogear (responderPreguntaID stack (GetListaDeRespuestas stack) id answer date (list .labels)))
                                "No existe pregunta con tal ID")))))

;ACCEPT
;Funcion: permite a un usuario con sesión iniciada en la plataforma aceptar una respuesta a una de sus preguntas.
;Dominio: stack
;Recorrido: stack con la respuesta aceptada entregando reward si es que tiene.
;Ejemplo:(((login stackoverflow "user" "pass" accept) 1) 1)
;Ejemplo:(((login (((login stackoverflow "user" "pass" reward) 1) 300) "user" "pass" accept) 1) 1)
(define (accept stack)
  (lambda (idPregunta)
    (lambda(idRespuesta)
      ;Existe usuario logeado
      (if (not (null? (GetActiveUsuario stack)))
       ;pregunta corresponde   
      (if(preguntaCorresponde? (GetListaDePreguntas stack) idPregunta (GetUsuarioLogeado stack))
         ;Si existe respuesta
         (if (existeRespuestaID? (GetListaDeRespuestas stack) idPregunta idRespuesta)
             (deslogear (asignarRewardUsuario
              ;Se asigna el reward al usuario de la respuesta
             (asignarRewardUsuario stack (getUsuarioAnswer (GetListaDeRespuestas stack) idPregunta idRespuesta) (+ 15 (getRewardPregunta (GetListaDePreguntas stack) idPregunta)))
             ;Se asigna el reward al usuario de la pregunta
             (GetUsuarioLogeado stack) (esNegativo(- 2 (getRewardPregunta (GetListaDePreguntas stack) idPregunta)) )))
             "No existe la respuesta entregada")
         "Pregunta no corresponde a usuario logeado\n")
      "No hay usario logeado \n"))))


;STACK -> STRING
;Funcion:recibe un stack y entrega una representación del mismo como un string posible de visualizar de forma comprensible al usuario
;Dominio: stack
;Recorrido: string del stack
;EJEMPLO:(display(stack->string stackoverflow))
(define stack->string (lambda (stack)
       (string-append
        (usuarios2String (GetListaDeUsuarios stack)) "\n"
        (preguntas2String (GetListaDePreguntas stack) (GetListaDeRespuestas stack)) "\n"
        )))

;Funcion: Nos permite convertir en string una lista que entregemos.
;Dominio: lista
;Recorrido:string de la lista
;Ejemplo:(usuarios2String (GetListaDeUsuarios stack))
(define usuarios2String
  (lambda (lista)
    (define recorrerUsuarios
      (lambda (lista string i)
        (cond
          [(null? lista) string]
          [else (recorrerUsuarios (cdr lista) (string-append string (number->string i)".-" (selectorDato (car lista) 0) "Reputación : " (~v (selectorDato (car lista) 2))"\n")(+ 1 i))])))   
  (if (null? lista)
  "No hay usuarios registrados \n"
  (recorrerUsuarios lista "Usuarios Registrados : \n" 1 ))))

;Funcion:Funcion para convertir las etiquetas en string.
;Dominio: lista x lista etiquetas
;Recorrido: lista string de etiquetas
(define etiquetas2String
  (lambda (lista string)
    (cond
          [(null? lista) string]
          [else (etiquetas2String (cdr lista) (string-append string (car lista) ", " ))])))

;Fucnion: Funcion para convertir las preguntas en string.
;Dominio: lista de preguntas
;Recorrido: lista de preguntas en string.
;Tipo de Recursión: cola.
(define preguntas2String
  (lambda (lista listaR)
    (define recorrerPreguntas
      (lambda (lista string i)
        (cond
          [(null? lista) string]
          [else (recorrerPreguntas (cdr lista)
          (string-append string (number->string i)".-" "ID : " (number->string (selectorDato (car lista) 0)) " Pregunta : " (selectorDato (car lista) 1) " Autor : " (selectorDato (car lista) 2)  (etiquetas2String (selectorDato (car lista) 3) " Etiquetas : ") " Fecha :" (selectorDato (car lista) 4) " Reward : "(number->string (selectorDato (car lista) 5))"\n"
                          (respuestas2String listaR (selectorDato (car lista) 0) ))(+ 1 i))])))   
  (if (null? lista)
  "No hay preguntas realizadas \n"
  (recorrerPreguntas lista "Preguntas realizadas : \n" 1 ))))
;Funcion: nos permite pasar a string la lista de respuestas 
;Dominio : lista de respuestas idPregunta
;Recorrido: lista de string
;Ejemplo: (respuestas2String (GetListaDeRespuestas stackoverflow) 1)
(define respuestas2String
  (lambda(lista idPregunta )
    (define recorrerRespuestas
      (lambda (lista string i)
        (cond
          [( null? lista) string]
          [else (recorrerRespuestas (cdr lista) (string-append string (number->string i)  ".- ID : "(number->string (selectorDato (car lista)0)) " Autor :" (selectorDato (car lista)1) " Respuesta :" (selectorDato (car lista)3) " Fecha : " (selectorDato (car lista)2) (etiquetas2String (selectorDato (car lista) 4) " Etiquetas : ") ) (+ i 1)) ])

        ))
    (cond
      ;Hasta que llege a ser nulo
      [(null? lista)]
      ;Si no es nulo sigo buscando el ultimo elemento (null) de la lista
      [(equal? (car(car lista)) idPregunta)  (recorrerRespuestas (cdr (car lista)) (string-append "Respuestas de la pregunta de ID : " (~v (car(car lista))) "\n") 1)  ]
      [else(respuestas2String (cdr lista) idPregunta)])))

;EJEMPLOS DE FUNCIONAMIENTO DE FUNCIONES:
;Primero debemos crear el stack que es nuestro TDA:
;(define stack (stackoverflow (list) (list) (list) (list)))

;FUNCION REGISTER:
;Funcion que se registra un usuario:
;1:(define stack1(register(register(register stack "user01" "pass01")"user02" "pass02")"user03" "pass03"))
;2:(define stack2(register (register stack "user01" "pass01")"user02" "pass02")
;3:(define stack3(register stack "user01" "pass01"))

;FUNCION LOGIN:
;Si el usuario se encuentra en el stack se mostrara que esta esperando
;los datos para realizar la funcion.
;1:(login stack1 "user01" "pass01" ask)
;si la contraseña esta errada solo mostrara que puede hacer funcion ask.
;2:(login stack1 "user02" "pass04" ask)
;Si no se encuentra solo mostrara que puede hacer funcion ask.
;3:(login stack1 "user06" "pass01" ask)


;FUNCION ASK:
; Si el usuario se encuentra en el stack y tiene bien la clave:
;1: (define stack4(((login stack1 "user01" "pass01" ask)(date))"question?" "C#"))
; Si el usuario no se encuentra registrado:
; Mostrara un error ya que no se encuentra el usuario.
;2:(define stack5(((login stack1 "user06" "pass01" ask)(date))"question?" "C#"))
; Pasa lo mismo si se encuentra el usuario pero la contraseña es correcta:
;3:(define stack6(((login stack1 "user01" "passss21" ask)(date))"question?" "C#"))

;FUNCION REWARD:
;El usuario puede dar recompensa por su pregunta:
;1:(define stack7 (((login stack4 "user01" "pass01" reward) 1) 0))
;Si el usuario falla en su nombre de usuario no podra hacerlo, ya que
;Se espera que ese usuario ingrese y haga una pregunta.
;2:(define stack8 (((login stack4 "user013" "pass01" reward) 1) 0))
;Si el usuario da recompensa a una pregunta en este caso id 3 y no existe
;Se muestra un mensaje con que no existe la pregunta.
;3:(define stack9 (((login stack4 "user01" "pass01" reward) 3) 0))

;FUNCION ANSWER:
;El usuario responda una respuesta a otro usuario.
;USUARIO responde a pregunta del stack4 de user01.
;1:(define stack10((((login stack4 "user02" "pass02" answer) (date)) 1) "mi respuesta" "C") )
;Si el usuario ingresa un id que no tenga pregunta, se muestra en pantalla
;que no existe id para esa pregunta.
;2:(define stack11((((login stack4 "user02" "pass02" answer) (date)) 3) "mi respuesta" "C") )
;Si el usuario ingresa mal a su cuenta para responder, se mostrara
;en pantalla un error.
;3:(define stack12((((login stack4 "user0233" "pass02" answer) (date)) 3) "mi respuesta" "C") )


;FUNCION ACCEPT:
;El usuario acepta la respuesta de otro usuario:
;Si no tiene reward el usuario que responde gana 15 puntos y el que genera respuesta 2 puntos.
;1:(define stack12(((login (((login stack10 "user01" "pass01" reward) 1) 0) "user01" "pass01" accept) 1) 1))
;Si otro usuario quiere aceptar la respuesta de la pregunta que el no ha generado,
;Se entrega un error
;2:(define stack13(((login (((login stack10 "user02" "pass02" reward) 1) 0) "user2" "pass02" accept) 1) 1))

;3:

;FUNCION STACK -> STRING:
;El stack se transforma en string.
;1:(display(stack->string stack12))
;Mostramos lista de usuarios con su reputacion.
;2:(display(stack->string stack1))
;3:

;Ahora todo un codigo generado:
;(define so (stackoverflow (list) (list) (list) (list)))
;(define so1(register(register(register so "user01" "pass01")"user02" "pass02")"user03" "pass03"))
;(define so2(((login so1 "user01" "pass01" ask)(date))"question?" "C#"))
;(define so3(((login so2 "user02" "pass02" ask)(date))"question2?" "C#"))
;(define so4 (((login so3 "user01" "pass01" reward) 1) 300))
;(define so5((((login so4 "user02" "pass02" answer) (date)) 1) "mi respuesta" "C") )
;(define so6((((login so5 "user03" "pass03" answer) (date)) 2) "mi respuesta2" "C"))
; Como usuario01 no tiene puntos de reward no se le otorgara al que responda.
;(define so7(((login (((login so6 "user01" "pass01" reward) 1) 300) "user01" "pass01" accept) 1) 1))
;(define so8(((login (((login so7 "user02" "pass02" reward) 2) 0) "user02" "pass02" accept) 2) 1))
;(display(stack->string so8))