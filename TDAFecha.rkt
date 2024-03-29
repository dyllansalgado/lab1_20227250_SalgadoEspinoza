#lang racket
;Implementación del TDA fecha
;CONSTRUCTOR
;descripción: Permite crear una fecha
;dom: entero X entero X entero
;rec: string
(define (fecha d m a)
  (if (and (integer? d) (integer? m) (integer? a)
           (> d 0) (> m 0) (< m 13) (not (= a 0))
           (<=  d (getDiasDelMes m a)))
      (fecha2String (list d m a))
      null
  )
)
;PERTENENCIA
;descripción: Función que permite determinar si un elemento cualquiera es del tipo fecha
;             se implementa a partir del constructor
;             evaluando el retorno
;dom: elemento de cualquier tipo
;rec: boolean
(define (fecha? f)
  (and (list? f) 
       (= (length f) 3)
       (not (null? (fecha (car f) (cadr f) (caddr f)))))
)

;SELECTORES
;descripción: Función que retorna el día en una fecha
;dom: fecha
;rec: entero
(define (getDia f)
   (if (fecha? f)
       (car f)
       0
   )
 )

;descripción: Función que retorna el mes en una fecha
;dom: fecha
;rec: entero
(define (getMes f)
 (if (fecha? f)
       (cadr f)
       0
   )  
)

;descripción: Función que retorna el año en una fecha
;dom: fecha
;rec: entero
(define (getAgno f)
 (if (fecha? f)
       (caddr f)
       0
   )
 )

;Modificadores
;descripción: Función que crea una nueva fecha a partir de una fecha de entrada reemplazando el valor correspondiente al día
;dom: fecha x entero
;rec: fecha
(define (setDia f nd)
  (if (fecha? f)
      (fecha nd (getMes f) (getAgno f))
      null
   )
 )

;descripción: Función que crea una nueva fecha a partir de una fecha de entrada reemplazando el valor correspondiente al mes
;dom: fecha x entero
;rec: fecha
(define (setMes f nm)
  (if (fecha? f)
      (fecha (getDia f) nm (getAgno f))
      null
   )
 )

;descripción: Función que crea una nueva fecha a partir de una fecha de entrada reemplazando el valor correspondiente al año
;dom: fecha x entero
;rec: fecha
(define (setAgno f na)
  (if (fecha? f)
      (fecha (getDia f) (getMes f) na)
      null
   )
 )

;OTRAS FUNCIONES
;descripción: función que transforma una fecha en string
;dom: fecha
;rec: string
(define (fecha->string f)
  (if (fecha? f)
      (string-append (number->string (getDia f)) " de " (getMonthName (getMes f)) " de " (number->string (getAgno f)))
      ""
   )
)

;descripción: función que retorna el siguiente año para una fecha
;dom: fecha
;rec: entero
(define (nextAgno f)
  (if (fecha? f)
      (setAgno f (+ 1 (getAgno f)))
      null
  )
 )
;representacion
;(entero X entero X entero)
;(list dia mes año) 
(define fecha2String
  (lambda (lista)
    (define (convertirLista lista string i)
      (if (null? lista)
      string
      (if (= i 2)
          (convertirLista (cdr lista)  (string-append string (~v (car lista))) (+ i 1))
          (convertirLista (cdr lista)  (string-append string (~v (car lista)) "/") (+ i 1))
          )
      ))
    (convertirLista lista "" 0)))

;LAS SIGUIENTES TRES FUNCIONES SON COMPLEMENTARIAS/AUXILIARES AL TDA. NO FORMAN PARTE DEL TDA FECHA, PERO
;EL TDA FECHA LAS EMPLEA PARA PODER REALIZAR SU TRABAJO. ESTAS FUNCIONES DE IGUAL FORMA PUEDEN
;SER EMPLEADAS INDEPENDIENTEMENTE DE LA EXISTENCIA DEL TDA FECHA, POR LO QUE NO EXISTE ACOPLAMIENTO CON EL TDA.
;POR OTRO LADO, EL TDA SEGUN LA IMPLEMENTACION REALIZADA A CONTINUACION, ESTA ACOPLADO A ESTAS FUNCIONES.

;descripción: función para determinar si un año es bisiesto
;dom: entero
;rec: boolean
(define (bisiesto? a)
  (if (and (integer? a) (not (= a 0)))
      (or (= (remainder a 400) 0)
              (and (= (remainder a 4) 0) (not (= (remainder a 100) 0))))
      #f
  )
)

;descripción: función para determinar los días de un mes
;dom: entero X entero
;rec: entero
(define (getDiasDelMes m a)
  (if (and (integer? m) (integer? a) (not (= a 0))
           (> m 0) (< m 13))
           (if (or (= m 1) (= m 3) (= m 5) (= m 7) (= m 8) (= m 10) (= m 12))
                31
                (if (= m 2)
                    (if (bisiesto? a)
                        29
                        28
                    )
                    30
                )
            )
           0
   )
 )

;descripción: función que transforma un mes entero a su nombre en string
;dom: entero
;rec: string
(define (getMonthName m)
      (cond ((not (and (integer? m) (> m 0) (< m 13))) "")
            ((= m 1) "Enero")
            ((= m 2) "Febrero")
            ((= m 3) "Marzo")
            ((= m 4) "Abril")
            ((= m 5) "Mayo")
            ((= m 6) "Junio")
            ((= m 7) "Julio")
            ((= m 8) "Agosto")
            ((= m 9) "Septiembre")
            ((= m 10) "Octubre")
            ((= m 11) "Noviembre")
            ((= m 12) "Diciembre")
       )
  )
(provide fecha)