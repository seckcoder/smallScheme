(library
  (Compiler remove-anonymous-lambda)
  (export remove-anonymous-lambda)
  (import
   (chezscheme)
   (Framework helpers)
   (Framework match)
   (Compiler my-helpers)
   )
  
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
  
(define who 'remove-anonymous-lambda)

(define strip-lambda
  (lambda (expr)
    (match expr
      [(lambda (,uvar* ...) ,[Expr -> body]) `(lambda ,uvar* ,body)]
      [,[Expr -> e] e])))

(define Expr
  (lambda (program)
    (match program
      [(false) '(false)]
      [(true)  '(true)]
      [(quote ,i) `(quote ,i)]
      [(if ,[p] ,[c] ,[a]) `(if ,p ,c ,a)]
      [(begin ,[e*] ... ,[p]) (make-begin `(,e* ... ,p))]
      [(let ([,uvar* ,[strip-lambda -> expr*]] ...) ,[expr])
       `(let ([,uvar* ,expr*] ...) ,expr)]
      [(letrec ([,uvar* ,[strip-lambda -> expr*]] ...) ,[expr])
       `(letrec ([,uvar* ,expr*] ...) ,expr)]
      [(lambda (,uvar* ...) ,[expr])
       (let ([label (unique-name 'anon)])
         `(letrec ([,label (lambda ,uvar* ,expr)]) ,label))]
      [(,prim ,[v*] ...) (guard (prim? prim)) `(,prim ,v* ...)]
      [,x (guard (uvar? x)) x]
      [,x (guard (or (label? x) (immediate? x))) x]
      [(,[expr] ,[expr*] ...) `(,expr ,expr* ...) ]
      [,e (errorf who "Expr ~s" e)])))


(define remove-anonymous-lambda
  (lambda (program)
    (match program
      [,[Expr -> prog] prog]
      [,e (errorf who "Program ~s" e)])))

);end library

