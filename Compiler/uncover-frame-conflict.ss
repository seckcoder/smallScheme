(library 
  (Compiler uncover-frame-conflict)
  (export  uncover-frame-conflict)
  (import 
    (Framework helpers)
    (Framework match)
    (Compiler my-helpers)
    (Compiler uncover-common)
    (chezscheme))


  ;; Boilerplate
  (define uncover-frame-conflict
    (lambda (pr)
      (match pr
        [(letrec ([,labels (lambda () ,[Body -> body*])] ...) ,[Body -> body])
         `(letrec ([,labels (lambda () ,body*)] ...) ,body)])))

  ;; Boilerplate
  (define Body
    (lambda (bod)
      (match bod
        [(locals (,uvars ...) (new-frames (,f* ...)  ,tail))
         (let-values ([(tail-conf call-live) (uncover frame-var? uvars tail)])
           `(locals ,(difference uvars call-live)
              (new-frames (,f* ...)
                (spills ,(difference call-live registers)
                  (frame-conflict ,tail-conf
                    (call-live ,(difference call-live registers) ,tail))))))])))
  )
