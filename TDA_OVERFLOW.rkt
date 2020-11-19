#lang racket
(require "funciones.rkt")
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
;Funcion:
;Dominio:
;Recorrido:
;Tipo recursion: natural
;Ejemplo: (preguntaCorresponde? (GetListaDePreguntas stack) id "user")
(define preguntaCorresponde?
  (lambda (lista id user)
    (cond
      ;Hasta que llege a ser nulo
      [( null? lista) #f]
      ;Si no es nulo sigo buscando el ultimo elemento (null) de la lista
      [(equal? (car(car lista)) id) (equal? (selectorDato (car lista) 2) user) ]
      [else(preguntaCorresponde? (cdr lista) id user)])))

;Funcion:
;Dominio:
;Recorrido:
;Tipo recursion: natural
;Ejemplo:(existePreguntaID? (GetListaDePreguntas stack) id )
(define existePreguntaID?(lambda(lista id)
                            (cond
                              ;Hasta que llege a ser nulo
                              [( null? lista) #f]
                              ;Si no es nulo sigo buscando el ultimo elemento (null) de la lista
                              [(equal? (car(car lista)) id) #t ]
                              [else(existePreguntaID? (cdr lista) id)])))
;Funcion:
;Dominio: lista de respuestas idPregunta idRespuesta.
;Recorrido:
;Tipo recursion:
;Ejemplo: (existeRespuestaID? (GetListaDeRespuestas stack) idPregunta idRespuesta)
(define existeRespuestaID?(lambda(lista idPregunta idRespuesta)
                            (cond
                              ;Hasta que llege a ser nulo
                              [( null? lista) #f]
                              ;Si no es nulo sigo buscando el ultimo elemento (null) de la lista
                              [(equal? (car(car lista)) idPregunta) (existePreguntaID? (cdr(car lista)) idRespuesta) ]
                              [else(existeRespuestaID? (cdr lista) idPregunta idRespuesta)])))

;SELECTORES
;selectores de stackoverflow
(define (GetListaDeUsuarios stack) (selectorDato stack 0))
(define (GetListaDePreguntas stack) (selectorDato stack 1))
(define (GetListaDeRespuestas stack) (selectorDato stack 2))
(define (GetActiveUsuario stack) (selectorDato stack 3))
(define (GetUsuarioLogeado stack) (selectorDato (selectorDato stack 3) 0))

;Funcion: Obtener cuanto reward da cierta pregunta dado el id de esta.
;Dominio:
;Recorrido:
;Ejemplo:(getRewardPregunta (GetListaDePreguntas stackoverflow) 1)
(define getRewardPregunta (lambda (lista idPregunta)
                            (selectorDato (selectorDato lista (getIndicePreguntaID lista idPregunta)) 5)))

;Funcion: Obtener el usuario de una respuesta
;Entrada: Recibe el id de la pregunta y el id de respuesta.
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
;Tipo de recursion: natural.
;Ejemplo:(getIndiceUsuario so "usuario")
(define getIndiceUsuario
  (lambda (stack usuario)
    (define buscarIndice
      (lambda (lista usuario i)
        (cond
          ;Hasta que llege a ser nulo
          [( null? lista) "no se encuentra indice de usuario"]
          ;Si no es nulo sigo buscando el ultimo elemento (null) de la lista
          [(equal? (car(car lista)) usuario) i]
          [else (buscarIndice (cdr lista) usuario (+ i 1))])))
    (buscarIndice (GetListaDeUsuarios stack) usuario 0)))

;Funcion: obtener indice de pregunta en una lista de respuestas
;Entrada:
;Salida:
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
;MODIFICADORES
;Funcion: Asigna el reward que se ha ingresado a un usuario.
;Entrada: stack X usuario X reward.
;Salida: Usuario con reward otorgado.
;Ejemplo:(asignarRewardUsuario stackoverflow "juan01" 100)
(define asignarRewardUsuario (lambda (stack usuario reward)
                               (cambiarDato stack 0 (cambiarDato (GetListaDeUsuarios stack) (getIndiceUsuario stack usuario) (cambiarDato (selectorDato (GetListaDeUsuarios stack) (getIndiceUsuario stack usuario)) 2 reward ))) ))
;Funcion:
;Entrada:
;Salida:
;Ejemplo:
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
;Entrada:
;Salida:
;Ejemplo:
(define asignarReward (lambda (pregunta reward)
                        (cambiarDato pregunta 5 reward)))
;Responder pregunta segun el id.
;Dominio:
;Recorrido:
;Ejemplo:
(define responderPreguntaID (lambda (stack listaRespuestas id respuesta date etiquetas)
                               ;caso de que la lista de respeustas a la pregunta este vacía o no exista la respuesta aún
                               (if (or (null? listaRespuestas) (equal? (getIndicePreguntaID listaRespuestas id) #f))
                                   (cambiarDato stack 2
                                    (añadirDato listaRespuestas (list id (list 1 (GetUsuarioLogeado stack) date respuesta etiquetas))))
                                   ;Caso de que si hayan respuestas
                                   (cambiarDato stack 2
                                   (cambiarDato listaRespuestas (getIndicePreguntaID listaRespuestas id)
                                    (añadirDato (selectorDato listaRespuestas (getIndicePreguntaID listaRespuestas id)) (list (length (selectorDato listaRespuestas (getIndicePreguntaID listaRespuestas id))) (GetUsuarioLogeado stack) date respuesta etiquetas)))))))

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

;Funcion : login permite al registro realizado poder ingresar.
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
                           "Contraseña incorrecta\n"
                         )
                        "El usuario entregado no se encuentra registrado\n"
                        )
                    "Datos entregados erroneos"
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
             (GetUsuarioLogeado stack) (- 2 (getRewardPregunta (GetListaDePreguntas stack) idPregunta)) ))
             "No existe la respuesta entregada")
         "Pregunta no corresponde a usuario logeado\n")
      "No hay usario logeado \n"))))


;STACK ->STRING
;Funcion:recibe un stack y entrega una representación del mismo como un string posible de visualizar de forma comprensible al usuario
;Dominio: stack
;Recorrido: string del stack
;EJEMPLO:(display(stack->string stackoverflow))
(define stack->string (lambda (stack)
       (string-append
        (usuarios2String (GetListaDeUsuarios stack)) "\n"
        (preguntas2String (GetListaDePreguntas stack) (GetListaDeRespuestas stack)) "\n"
        ))
)