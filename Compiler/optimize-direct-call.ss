(library
  (Compiler optimize-direct-call)
  (export optimize-direct-call)
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
;;  OUTPUT:
;;
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

(define who 'optimize-direct-call)

(define Expr
  (lambda (program)
    (match program
      [(false) '(false)]
      [(true)  '(true)]
      [(quote ,i) `(quote ,i)]
      [(if ,[p] ,[c] ,[a]) `(if ,p ,c ,a)]
      [(begin ,[e*] ... ,[p])  (make-begin  `(,e* ... ,p))]
      [(let ([,uvar ,[expr*]] ...) ,[expr]) `(let ([,uvar ,expr*] ...) ,expr)]
      [(letrec ([,binding* ,[expr*]] ...) ,[expr]) `(letrec ([,binding* ,expr*] ...) ,expr)]
      [(,prim ,[v*] ...) (guard (prim? prim)) `(,prim ,v* ...)]
      [((lambda (,x* ...) ,[body]) ,args* ...)  (guard (eq? (length x*) (length args*)))
                                                (let ([bindings (map (lambda (b e) `(,b ,e)) x* (Expr args*))])
                                                  `(let ,bindings ,body))]
      [(lambda (,x* ...) ,[body]) `(lambda ,x* ,body)]
      [(,[v] ,[v*] ...)  `(,v ,v* ...)]
      [,x (guard (or (uvar? x) (label? x) (immediate? x))) x]
      [(,uvar ,[rand*] ...) (guard (uvar? uvar))  `(,uvar  ,uvar ,rand* ...)]
      [(,[rator] ,[rand*] ...) 
       (let ([tmp (unique-name 'tmp)])
         `(let ([,tmp ,rator])
            (,tmp ,tmp ,rand* ...)))]
      [,e (errorf who "Expr ~s" e)])))

  (define optimize-direct-call
    (lambda (program)
      (match program
        [,[Expr -> prog] prog]
        [,e (errorf who "Program ~s" e)])))

  );; end lib
