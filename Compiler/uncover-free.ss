(library
  (Compiler uncover-free)
  (export uncover-free)
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
;;  |(letrec ([uvar (lambda (uvar*) (free freevars Expr))]*) Expr)
;;  |(prim Expr*)
;;  |(Expr Expr*)
;;immediate 
;;  | () 
;;  | #t 
;;  | #f


  (define who 'uncover-free)

  

    ;;expose free handles all of our bindings in letrecs and generates
    ;;a free form wrapped around lambda bodies.
    (define expose-free
      (lambda (b)
        (match b
          [(,label (lambda (,uvar* ...) ,[Expr -> sx fx body]))
           (let ([new (make-set (difference (flatten* fx) (union* `(,uvar* ...) (flatten* sx))))])
             (values (flatten* sx) new `(,label (lambda (,uvar* ...) (free ,new ,body)))))]
          [(,label ,[Expr -> sx fx expr])
           (values (flatten* sx) (flatten* fx) `(,label ,expr))]
          [,e (errorf who "expose-free ~s" e)])))

    (define Expr
      (lambda (program)
        (match program
          [(false) (values '() '() '(false))]
          [(true) (values '() '() '(true))]
          [(quote ,i) (values '() '() `(quote ,i))]
          [(if ,[sp fp p] ,[sc fc c] ,[sa fa a])
           (values (union* sp sc sa) (union* fp fc fa) `(if ,p ,c ,a))]
          [(begin ,[se* fe* e*] ... ,[sp fp p])
           (values (union* se* sp) (union* fe*  fp) (make-begin `(,e* ... ,p)))]
          [(let ([,uvar* ,[sx* fx* expr*]] ...) ,[sx fx expr])
           (values (union* uvar* sx* sx) (difference (union* fx* fx) uvar*) `(let ([,uvar* ,expr*] ...) ,expr))]
          [(letrec (,b* ...) ,[sx fx expr])
           (let*-values ([(names) (map car b*)]
                         [(s f b) (if (null? b*) (values '() '() '()) (map-values expose-free b*))])
                        (values (union* sx s names) (union* f fx) `(letrec ,b ,expr)))]
          [(,prim ,[sv* fv* v*] ...) (guard (prim? prim))
                                     (values (make-set (flatten* sv*)) `(,fv* ...) `(,prim ,v* ...))]
          [,x (guard (uvar? x))
              (values  '() `(,x) x)]
          [,x (guard (or (label? x) (immediate? x)))
              (values '() '() x)]
          [(,[sx fx expr] ,[sx* fx* expr*] ...)
           (values (union* sx sx*) (union* fx fx*) `(,expr ,expr* ...))]
          [,e (errorf who "Expr ~s" e)])))
  
  (define uncover-free
    (lambda (program)
      (match program
        [,[Expr -> hukarz env prog] prog]
        [,e (errorf who "Program ~s" e)])))

  );end library
