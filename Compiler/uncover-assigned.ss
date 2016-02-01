(library
  (Compiler uncover-assigned)
  (export uncover-assigned)
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
;;  |(set! uvar Expr)
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
;;  |(letrec ([uvar (lambda (uvar*) (assigned avars Expr))]*) Expr)
;;  |(set! uvar Expr)
;;  |(prim Expr*)
;;  |(Expr Expr*)
;;immediate 
;;  | () 
;;  | #t 
;;  | #f
 
(define who 'uncover-assigned)

;;expose assigned handles all of our bindings in letrecs and generates
;;a assigned form wrapped around lambda bodies.
(define expose-assigned
  (lambda (b)
    (match b
      [(,label (lambda (,uvar* ...) ,[Expr -> sx fx body]))
       (let ([new (intersection uvar* (flatten* fx))])
         (values (flatten* sx) (flatten* fx) `(,label  (lambda (,uvar* ...) (assigned ,new ,body)))))]
      [(,label ,[Expr -> sx fx expr])
       (let ([new (union* fx sx)])
         (values (flatten* sx) new `(,label ,expr)))]
      [,e (errorf who "expose-assigned ~s" e)])))

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
      [(letrec ([,uvar* ,[sx* fx* expr*]] ...) ,[sx fx expr])
       (values (union* uvar* sx* sx) (difference (union* fx* fx) uvar*)
               `(letrec ([,uvar* ,expr*] ...)
                  (assigned ,(flatten* fx) ,expr)))]
      [(let (,b* ...) ,[sx fx expr])
       (let*-values ([(names) (map car b*)]
                     [(s f b) (if (null? b*) (values '() '() '()) (map-values expose-assigned b*))])
                    (values fx (union* f fx) `(let ,b (assigned ,(intersection names fx) ,expr))))]
      [(,prim ,[sv* fv* v*] ...) (guard (prim? prim))
                                 (values (make-set (flatten* sv*)) `(,fv* ...) `(,prim ,v* ...))]
      [(lambda ,uvar* ,[sx fx expr]) (values sx fx `(lambda ,uvar* (assigned ,(intersection uvar* (flatten* fx)) ,expr)))]
      [(set! ,uvar ,[sx fx expr])
       (values sx (set-cons uvar fx) `(set! ,uvar ,expr))]
      [,x (guard (ort x uvar? label? immediate?)) (values '() '() x)]
      [(,[sx fx expr] ,[sx* fx* expr*] ...)
       (values (union* sx sx*) (union* fx fx*) `(,expr ,expr* ...))]
      [,e (errorf who "Expr ~s" e)])))

(define uncover-assigned
  (lambda (program)
    (match program
      [,[Expr -> hukarz env prog] prog]
      [,e (errorf who "Program ~s" e)])))

) ;; end lib
