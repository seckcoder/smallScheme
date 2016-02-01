 (library 
  (Compiler Optimizations uncover-well-known)
  (export uncover-well-known)
  (import 
    (Framework match)
    (Framework helpers)
    (Compiler my-helpers)
    (chezscheme))

  (define who 'uncover-well-known)

(define-who uncover-well-known
  (define primitives
    '(+ - * <= < = >= > boolean? car cdr cons eq? fixnum?
      make-vector null? pair? procedure? set-car! set-cdr! vector?
      vector-length vector-ref vector-set! void))
  (define Lambda
    (lambda (x)
      (match x
        [(lambda (,fml* ...)
           (bind-free (,cp ,free* ...)
             ,[Expr -> s*]))
         `(lambda (,fml* ...)
           (bind-free (,cp ,free* ...)
             ,s*))]
        [,x (error who "invalid Lambda ~s" x)])))
  (define get-well-known
    (lambda (x)
      (match x
        [(,f ,lab) (values `(,f ,lab) '())]
        [() (values '() '())]
        [(,f ,lab ,cl ...) (values `(,f ,lab ,cl ...) `(,f))]
        [,e (errorf who "get-well-known ~s" e)])))
  (define Expr
    (lambda (x)
      (match x
        [,label (guard (label? label)) label]
        [,uvar (guard (uvar? uvar)) uvar]
        [(quote ,imm) `(quote ,imm) ]
        [(if ,[test-s*] ,[conseq-s*] ,[altern-s*])
         `(if ,test-s* ,conseq-s* ,altern-s*)]
        [(begin ,[s**] ... ,[s*]) `(begin ,s** ... ,s*)]
        [(let ([,lhs* ,[s**]] ...) ,[s*]) 
         `(let ([,lhs* ,s**] ...) ,s*)]
        [(letrec ([,llabel* ,[Lambda -> s**]] ...)
           (closures (,[get-well-known -> clos* known] ...)
             ,[s*]))
          `(letrec ([,llabel* ,s**] ...)
           (closures (,clos* ...)
             (well-known (,known ... ...) ;; double wrap these
                     ,s*)))]
        [(,prim ,[s**] ...)
         (guard (memq prim primitives)) `(,prim ,s** ...)]
        [(,[s*] ,[s**] ...) `(,s* ,s** ...)]
        [,x (errorf who "invalid Expr ~s" x)])))
  (lambda (x)
    (let ([s* (Expr x)])
      s*)))




  ) ;; end lib
