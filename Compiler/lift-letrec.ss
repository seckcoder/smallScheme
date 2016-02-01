(library
  (Compiler lift-letrec)
  (export lift-letrec)
  (import
    (chezscheme)
    (Framework helpers)
    (Framework match)
    (Compiler my-helpers))

;;  INPUT
;;
;;Program
;;  |Expr
;;Expr
;;  |label
;;  |uvar
;;  |(quote Immediate)
;;  |(if Expr Expr Expr)
;;  |(begin Expr* Expr)
;;  |(let ([uvar Expr]*) Expr)
;;  |(letrec ([label (lambda (uvar*) Expr)]*) Expr)
;;  |(prim Expr*)
;;  |(Expr Expr*)
;;immediate 
;;  | () 
;;  | #t 
;;  | #f
;;  
;;  OUTPUT
;;
;;Program
;;  |Expr
;;Expr
;;  |label
;;  |uvar
;;  |(quote Immediate)
;;  |(if Expr Expr Expr)
;;  |(begin Expr* Expr)
;;  |(let ([uvar Expr]*) Expr)
;;  |(letrec ([label (lambda (uvar*) Expr)]*) Expr)
;;  |(prim Expr*)
;;  |(Expr Expr*)
;;immediate 
;;  | () 
;;  | #t 
;;  | #f

  (define who 'lift-letrec)

  (define Expr
    (lambda (program)
      (match program
        [(false) (values '() '(false))]
        [(true) (values '() '(true))]
        [(quote ,i) (values '() `(quote ,i))]
        [(if ,[letp p] ,[letc c] ,[leta a]) (values `(,letp ... ,letc ... ,leta ...) `(if ,p ,c ,a))]
        [(begin ,[lete* e*] ... ,[letp p])  
         (values `(,lete* ... ... ,letp ...) (make-begin `(,e* ... ,p)))]
        [(let ([,uvar ,[letx* expr*]] ...) ,[letx expr]) 
         (values `(,letx* ... ...  ,letx ...) `(let ([,uvar ,expr*] ...) ,expr))]
        [(letrec ([,label* (lambda ,uvar* ,[letx* expr*])] ...) ,[letx expr])
         (values `([,label* (lambda ,uvar* ,expr*)] ... ,letx* ... ... ,letx ...) expr)]
        [(,prim ,[letv* v*] ...) (guard (or (pred-prim? prim) (effect-prim? prim) (value-prim? prim) (proc-prim? prim)))
                                 (values `(,letv* ... ...) `(,prim ,v* ...))]
        [,x (guard (or (uvar? x) (label? x) (immediatefixnum? x))) (values '() x)]
        [(,[letx expr] ,[letx* expr*] ...) (values `(,letx ... ,letx* ... ...) `(,expr ,expr* ...))]
        [,e (errorf who "Expr ~s" e)])))

  (define lift-letrec
    (lambda (program)
      (match program
        [,[Expr -> letprog prog] `(letrec ,letprog ,prog)]
        [,e (errorf who "lift-letrec ~s\n" e)])))

  (define proc-prim?
    (lambda (x)
      (and  (memq x '(make-procedure procedure-ref procedure-code procedure-set!)) #t)))

  );end library

