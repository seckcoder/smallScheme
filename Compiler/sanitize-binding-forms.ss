(library
  (Compiler sanitize-binding-forms)
  (export sanitize-binding-forms)
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
;;  |(let ([uvar Expr]*) Expr)
;;  |(letrec ([uvar (lambda (uvar*) Expr)]*) Expr)
;;  |(prim Expr*)
;;  |(Expr Expr*)
;;immediate 
;;| | () 
;;| | #t 
;;| | #f
;;
;; OUTPUT:
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

(define who 'sanitize-binding-forms)

(define sanitize-lambda
  (lambda (bindings)
    (cond
      [(null? bindings) (values `() `())]
      [(pair? (car bindings))
       (let-values ([(l b) (sanitize-lambda (cdr bindings))])
         (if (and
               (pair? (cadar bindings))
               (eq?  'lambda (caadar bindings)))
           (values `(,(car bindings) ,@l) b)
           (values l `(,(car bindings) ,@b))))])))


(define Expr
  (lambda (program)
    (match program
      [(false) '(false)]
      [(true)  '(true)]
      [(quote ,i) `(quote ,i)]
      [(if ,[p] ,[c] ,[a]) `(if ,p ,c ,a)]
      [(begin ,[e*] ... ,[p]) (make-begin `(,e* ... ,p))]
      [(let ([,label ,[expr*]] ...) ,[expr])
       (let*-values ([(bindings) `([,label ,expr*] ...)]
                     [(l b) (sanitize-lambda bindings)])
                    (cond
                      [(and (null? l) (null? b) `(let () ,expr))]
                      [(null? l) `(let ,b ,expr)]
                      [(null? b) `(letrec ,l ,expr)]
                      [else `(letrec ,l (let ,b ,expr))]))]
      [(letrec ([,uvar* ,[expr*]] ...) ,[expr])
       `(letrec ([,uvar* ,expr*] ...) ,expr)]
      [(lambda (,uvar* ...) ,[body]) `(lambda ,uvar* ,body)]
      [(,prim ,[v*] ...) (guard (prim? prim)) `(,prim ,v* ...)]
      [(,[expr] ,[expr*] ...) `(,expr ,expr* ...) ]
      [,x (guard (uvar? x)) x]
      [,x (guard (or (label? x) (immediate? x))) x]
      [,e (errorf who "Expr ~s" e)])))

(define sanitize-binding-forms
  (lambda (program)
    (match program
      [,[Expr -> prog] prog]
      [,e (errorf who "Program ~s" e)])))

);end library

