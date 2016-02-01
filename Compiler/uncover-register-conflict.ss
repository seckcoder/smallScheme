
(library 
  (Compiler uncover-register-conflict)
  (export  uncover-register-conflict)
  (import 
    (Framework helpers)
    (Framework match)
    (Compiler my-helpers)
    (Compiler uncover-common)
    (chezscheme))

(define who 'uncover-register-conflict)

  ;; Boilerplate
  (define uncover-register-conflict
    (lambda (pr)
      (match pr
        [(letrec ([,labels (lambda () ,[Body -> body*])] ...) ,[Body -> body])
         `(letrec ([,labels (lambda () ,body*)] ...) ,body)]
        [,e (errorf who "Error with ~s" e)])))

  ;; Boilerplate
  (define Body
    (lambda (bod)
      (match bod
        [(locals (,uv* ...)
           (ulocals (,uloc* ...)
             (locate (,assed ...)
               (frame-conflict ,cf ,tl))))
         (let-values ([(tail-conf call-live)  (uncover register? 
                                                   `(,uv* ... ,uloc* ...) tl)])
          `(locals (,uv* ...)
           (ulocals (,uloc* ...)
             (locate (,assed ...)
               (frame-conflict ,cf 
                (register-conflict ,tail-conf ,tl))))))]
        [(locate (,assed ...) ,tl) `(locate (,assed ...) ,tl)]
        [,else (errorf who "Error in Body with ~s" else)])))
    )
