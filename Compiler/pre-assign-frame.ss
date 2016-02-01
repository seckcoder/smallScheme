(library 
  (Compiler pre-assign-frame)
  (export pre-assign-frame)
  (import 
    (Compiler assign-frame) ;; for get-homes
    (Compiler my-helpers)
    (Framework match)
    (Framework helpers)
    (chezscheme))

  (define who 'pre-assign-frame)


  (define pre-assign-frame  
    (lambda (program)
      (match program
        [(letrec ([,label* (lambda () ,[Body*])] ...) ,[Body])
         `(letrec ([,label* (lambda ()  ,Body*)]  ...)  ,Body)]
        [(locals ,local*
           (new-frames ,frame*
             (spills ,spill*
               (frame-conflict ,fv-ct 
                 (call-live ,cl ,tail)))))
         (let ([home* (get-homes spill* fv-ct '())])
           `(locals ,local* 
              (new-frames ,frame*
                (locate ,home*
                  (frame-conflict ,fv-ct
                    (call-live ,cl ,tail))))))]
        [,e (error who "pre-assign ~s" e)])))
  ) ;; end lib
