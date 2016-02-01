(library
  (Compiler optimize-known-call)
  (export optimize-known-call)
  (import
   (chezscheme)
   (Framework helpers)
   (Framework match)
   (Compiler my-helpers))

;;  INPUT: 
;;
;;Program
;;  |Expr
;;Expr
;;  |uvar
;;  |(quote Immediate)
;;  |(if Expr Expr Expr)
;;  |(begin Expr* Expr)
;;  |Lambda
;;  |(let ([uvar Expr]*) Expr)
;;  |(letrec ([uvar Lambda]*) Expr)
;;  |(prim Expr*)
;;  |(Expr Expr*)
;;Lambda
;;  |(lambda (uvar*) Expr)
;;immediate 
;;  | () 
;;  | #t
;;  | #f
;;
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
;;  |(letrec ([uvar (lambda (uvar*) Expr)]*) Expr)
;;  |(prim Expr*)
;;  |(Expr Expr*)
;;immediate 
;;  | () 
;;  | #t
;;  | #f

(define who 'optimize-known-call)

(define gen-env
  (lambda (program)
    (cond
      [(null? program) '()]
      [(eq? (car program) 'letrec)
       (append (extend-env (car (cdaddr program))) (gen-env (cdr program)))]
      [(pair? (car program)) (append (gen-env (car program)) (gen-env (cdr program)))]
      [else (gen-env (cdr program))])))

(define extend-env
  (lambda (cl)
    (cond
      [(null? cl) '()]
      [else (cons `(,(caar cl) . ,(cadar cl)) (extend-env (cdr cl)))])))

(define Expr
  (lambda (program env)
    (match program
      [(false) '(false)]
      [(true)  '(true) ]
      [(quote ,i) `(quote ,i)]
      [(if ,[p] ,[c] ,[a]) `(if ,p ,c ,a)]
      [(begin ,[e*] ... ,[p]) `(begin ,e* ... ,p)]
      [(let ([,uvar ,[expr*]] ...) ,[expr])
       `(let ([,uvar ,expr*] ...) ,expr)]
      [(letrec ([,uvar (lambda  ,uvar* (bind-free ,freevars*  ,[expr*]))] ...) (closures ,cl* ,[expr]))
       `(letrec ([,uvar (lambda  ,uvar* (bind-free ,freevars*  ,expr*))] ...) (closures ,cl* ,expr))]
      [(,prim ,[v*] ...) (guard (prim? prim)) `(,prim ,v* ...)]
      [,x (guard (uvar? x)) x]
      [,x (guard (or (label? x) (immediate? x))) x]
      [(,[expr] ,[expr*] ...)
       (cond
         [(and (uvar? expr) (assq expr env)) => (lambda (alist) `(,(cdr alist) ,expr* ...))]
         [else `(,expr ,expr* ...)])]
      [,e (errorf who "Expr ~s" e)])))


(define optimize-known-call
  (lambda (program)
    (match program
      [,prog
        (Expr prog (gen-env prog))]
      [,e (errorf who "Program ~s" e)])))

);end library

