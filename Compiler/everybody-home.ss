(library 
  (Compiler everybody-home)
  (export everybody-home?)
  (import 
    (Framework match)
    (Framework helpers)
    (chezscheme))


  ;; all we do here is see if everything has been assigned.
  ;;If it has, we say were one and stop iterating,, OTW we 
  ;;keeep on iterating until weve assigned everything. This 
  ;;code was taken from the problem description for a5  
 (define-who everybody-home?
  (define all-home?
    (lambda (body)
      (match body
        [(locals (,local* ...)
           (ulocals (,ulocal* ...)
             (spills (,spill* ...)
               (locate (,home* ...)
                 (frame-conflict ,ct ,tail))))) #f]
        [(locate (,home* ...) ,tail) #t]
        [,x (error who "invalid Body ~s" x)])))
  (lambda (x)
    (match x
       [(letrec ([,label* (lambda () ,body*)] ...) ,body)
        (andmap all-home? `(,body ,body* ...))]
       [,x (error who "invalid Program ~s" x)])))
  
  )
