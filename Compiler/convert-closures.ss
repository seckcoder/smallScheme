(library
  (Compiler convert-closures)
  (export convert-closures)
  (import
    (Framework helpers)
    (Framework match)
    (Compiler my-helpers)
    (chezscheme))


;;  INPUT:
;;  
;;Program
;;  |Expr
;;Expr
;;  |uvar
;;  |(quote Immediate)
;;  |(if Expr Expr Expr)
;;  |(begin Expr* Expr)
;;  |(let ([uvar Expr]*) Expr)
;;  |(letrec ([uvar (lambda (uvar*) (free freevars Expr))]*) Expr)
;;  |(prim Expr*)
;;  |(Expr Expr*)
;;immediate 
;;  | () 
;;  | #t 
;;  | #f
;;  
;;  OUTPUT:
;;  
;;Program
;;  |Expr
;;Expr
;;  |uvar
;;  |(quote Immediate)
;;  |(if Expr Expr Expr)
;;  |(begin Expr* Expr)
;;  |(let ([uvar Expr]*) Expr)
;;  |(letrec ([uvar (lambda (uvar*) (bind-free (freevars*) Expr))]*) (closures (closure*) Expr))
;;  |(prim Expr*)
;;  |(Expr Expr*)
;;immediate
;;  | ()
;;  | #t 
;;  | #f

  (define who 'convert-closures)

  (define make-closure
    (lambda (block)
      (match block
        [(,label (lambda ,uvar* (free ,freevars ,[Expr -> expr])))
         (let ([codepointer (unique-name 'cp)]
               [newlab (unique-label label)])
           (values `(,label ,newlab ,freevars ...)
                   `(,newlab (lambda (,codepointer ,uvar* ...)
                               (bind-free (,codepointer ,freevars ...) ,expr)))))]
        [,e (errorf who "Invalid letrec binding ~s" e)]))) 

  (define Expr
    (lambda (program)
      (match program
        [(false) '(false)]
        [(true)  '(true)]
        [(quote ,i) `(quote ,i)]
        [(if ,[p] ,[c] ,[a]) `(if ,p ,c ,a)]
        [(begin ,[e*] ... ,[p])  (make-begin  `(,e* ... ,p))]
        [(let ([,uvar ,[expr*]] ...) ,[expr]) `(let ([,uvar ,expr*] ...) ,expr)]
        [(letrec (,[make-closure -> closure* block*] ...) ,[expr])
         `(letrec ,block* (closures ,closure* ,expr))]
        [(,prim ,[v*] ...) (guard (or (pred-prim? prim) (effect-prim? prim) (value-prim? prim) (equal? prim 'procedure?)))
                           `(,prim ,v* ...)]
        [,x (guard (or (uvar? x) (label? x) (immediatefixnum? x))) x]
        [(,uvar ,[rand*] ...) (guard (uvar? uvar))  `(,uvar  ,uvar ,rand* ...)]
        [(,[rator] ,[rand*] ...) 
         (let ([tmp (unique-name 'tmp)])
           `(let ([,tmp ,rator])
              (,tmp ,tmp ,rand* ...)))]
        [,e (errorf who "Expr ~s" e)])))



  (define convert-closures
    (lambda (program)
      (match program
        [,[Expr -> prog] prog]
        [,e (errorf who "convert-closures ~s" e)])))

  );; end lib
