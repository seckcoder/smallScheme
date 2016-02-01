(library
  (Compiler introduce-procedure-primitives)
  (export introduce-procedure-primitives)
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
  ;;  |(letrec ([uvar (lambda (uvar*) (bind-free (freevars*) Expr))]*) (closures (closure*) Expr))
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

  (define who 'introduce-procedure-primitives)

  (define init-args
    (lambda (args i env label)
      (cond
        [(null? args) '()]
        [else
          (cons `(procedure-set! ,(Expr label env) (quote ,i) ,(Expr (car args) env))
                (init-args (cdr args) (add1 i) env label))]))) ;;todo

  (define init-closure
    (lambda (closure env)
      (cond 
        [(null? closure) (values '() '())]
        [(pair? (car closure))
         (let-values ([(c) (car closure)] [(procs inits) (init-closure (cdr closure) env)])
           (values
             `((,(car c) (make-procedure ,(cadr c) (quote ,(length (cddr c))))) ,@procs) ;;bindings
             `(,(init-args (cddr c) 0 env (car c)) ,@inits)))]
        [else (errorf who "init-clos")]))) ;;inits

  (define replace-uvar
    (lambda (uvar i env cp)
      (cond
        [(null? env) uvar]
        [(eq? uvar (car env)) `(procedure-ref ,cp (quote ,i))]
        [else (replace-uvar uvar (add1 i) (cdr env) cp)])))


  (define Expr
    (lambda (program env)
      (match program
        [(quote ,i) `(quote ,i)]
        [(if ,[p] ,[c] ,[a]) `(if ,p ,c ,a)]
        [(begin ,[e*] ... ,[p])  (make-begin  `(,e* ... ,p))]
        [(let ([,uvar ,[expr*]] ...) ,[expr]) `(let ([,uvar ,expr*] ...) ,expr)]
        [(letrec ([,uvar (lambda  ,uvar* (bind-free ,freevars*  ,expr*))] ...) (closures ,cl* ,[expr]))
         (let*-values ([(procs inits) (init-closure cl* env)]
                       [(expr*^) (map Expr expr* freevars*)]
                       [(expr^)  (make-begin `(,inits ... ... ,expr))])
           `(letrec ([,uvar (lambda ,uvar* ,expr*^)] ...) (let ,procs ,expr^)))]
        [(,prim ,[v*] ...) (guard (prim? prim)) `(,prim ,v* ...)]
        [,x (guard (uvar? x)) (if (null? env) x  (replace-uvar x 0 (cdr env) (car env)))]
        [(,uvar ,[uvar^] ,[rand*] ...) 
         (guard (uvar? uvar))
         `((procedure-code ,(Expr uvar env)) 
           ,uvar^ 
           ,(map (lambda (x) 
                   (if (eq? uvar x) 
                     `(procedure-ref ,uvar) x)) 
                 rand*) ...)]
        [(,label ,uvar ,[rand*] ...) 
         (guard (label? label) (uvar? uvar)) ;;this could happen if we ran optimize-know-call
         `(,label ,(Expr uvar env) 
                  ,(map (lambda (x) 
                          (if (eq? uvar x) 
                            `(procedure-ref ,uvar) x)) 
                        rand*) ...)]
        [(,notuvar ,notuvar^ ,[rand*] ...) `(,notuvar ,notuvar^ ,rand* ...)]
        [() '()]
        [,e (errorf who "Expr ~s" e)])))

  (define introduce-procedure-primitives
    (lambda (program)
      (match program
        [,prog (Expr prog '())]
        [,e (errorf who "Program ~s" e)])))

  );; end lib

