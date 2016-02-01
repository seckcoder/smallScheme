(library
  (Compiler  purify-letrec)
  (export purify-letrec)
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
;;  |(quote datum)
;;  |(if Expr Expr Expr)
;;  |(begin Expr* Expr)
;;  |(lambda (uvar*) Expr)
;;  |(let ([uvar Expr]*) Expr)
;;  |(letrec ([uvar Expr]*) Expr)
;;  |(set! uvar Expr)
;;  |(prim Expr*)
;;  |(Expr Expr*)
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
;;  |Lambda
;;  |(let ([uvar Expr]*) Expr)
;;  |(letrec ([uvar Lambda]*) Expr)
;;  |(set! uvar Expr)
;;  |(prim Expr*)
;;  |(Expr Expr*)
;;Lambda
;;  |(lambda (uvar*) Expr)
;;immediate 
;;  | () 
;;  | #t 
;;  | #f

(define who 'purify-letrec)

(define not-app
  (lambda (e symbolok)
    (cond
      [(or (symbol? e) (immediate? e)) #t]
      [(pair? (car e)) (and (not-app (car e) #f) (not-app (cdr e) #f))]
      [(eq? (car e) 'lambda) #t]
      [(prim? (car e)) (not-app (cdr e) #t)]
      [(and (symbol? (car e)) symbolok) (not-app (cdr e) #t)]
      [else #f])))

(define simple?
  (lambda (bound b expr)
    (match b
      [(,binding (lambda ,uvar* (assigned ,assgns ,body))) #f]
      [(,binding ,e) (and (simple?  bound e expr) (not-set! binding e) (not-set! binding expr))]
      [,e (and 
            (andmap (lambda (var) (if (list? e) (not (member? var e)) (not (member? var `(,e))))) bound)
            (not-app e #f))])))

(define not-set! 
  (lambda (uvar e)
    (cond
      [(atom? e) #t]
      [(eq? (car e) 'set!) (and (not (eq? (cadr e) uvar)) (not-set! uvar (caddr e)))]
      [(pair? (car e)) (and (not-set! uvar (car e)) (not-set! uvar (cdr e)))]
      [else (not-set! uvar (cdr e))])))

(define lambda?
  (lambda (bound b expr)
    (match b
      [(,binding (lambda (,uvar* ...) (assigned ,assgns ,body)))
       (and
         (not (member? binding (difference  assgns uvar*)))
         (not-set! binding body)
         (not-set! binding expr))]
      [,x #f])))


(define sort-letrec
  (lambda (bound bindings body)
    (cond
      [(null? bindings) (values '() '() '() '())]
      [(simple? bound (car bindings) body)
       (let-values ([(s l c x!) (sort-letrec bound (cdr bindings) body)])
         (values `(,(car bindings) ,@s) l c x!))]
      [(lambda? bound (car bindings) body)
       (let-values ([(s l c x!) (sort-letrec bound (cdr bindings) body)])
         (values s `(,(car bindings) ,@l) c x!))]
      [else
        (let-values ([(s l c x!) (sort-letrec bound (cdr bindings) body)])
          (values s l `(,(car bindings) ,@c) 
                  (flatten* `(,(cond
                                 [(member* 'assigned (car bindings)) => (lambda (x) (cadr x))]
                                 [else '()])
                               ,@x! ,(caar bindings)))))])))

(define populate-complex
  (lambda (c)
    (cond
      [(null? c) '()]
      [else
        (let* ([binds (map (lambda (b) `(,(unique-name 'x) ,(cadr b))) c)]
               [set!s (map (lambda (name b) `(set! ,(car name) ,(car b))) c binds)])
          `((let ,binds
              (assigned () ,(make-begin set!s)))))])))

(define Expr
  (lambda (program)
    (match program
      [(false) '(false)]
      [(true)  '(true)]
      [(quote ,i) (guard (immediate? i)) `(quote ,i)]
      [(if ,[p] ,[c] ,[a]) `(if ,p ,c ,a)]
      [(begin ,[e*] ... ,[p]) (make-begin `(,e* ... ,p))]
      [(let ([,uvar* ,[expr*]] ...) (assigned (,asgns ...) ,[expr]))
       `(let ([,uvar* ,expr*] ...) (assigned ,asgns ,expr))]
      [(letrec (,expr* ...) (assigned (,asgns ...) ,[expr]))
       (let-values ([(s l c x!) (sort-letrec (map car expr*) expr* expr)])
         `(let (,s ...)
            (assigned ()
                      (let ,(map (lambda (b) `(,(car b) (void))) c)
                        (assigned (,x! ...)
                                  ,(if (null? l)                                                     
                                     (make-begin `(,(populate-complex c) ... ,expr))
                                     `(letrec ,(reverse `(,l ...))
                                        ,(make-begin `(,(populate-complex c) ... ,expr)))))))))]
      [(lambda (,uvar* ...) (assigned ,asgns ,[expr])) `(lambda ,uvar* (assigned ,asgns ,expr))]
      [(set! ,uvar ,[expr]) `(set! ,uvar ,expr)]
      [(,prim ,[v*] ...) (guard (prim? prim)) `(,prim ,v* ...)]
      [,x (guard (or (uvar? x) (label? x) (immediate? x))) x]
      [(,[expr] ,[expr*] ...) `(,expr ,expr* ...) ]
      [,e (errorf who "Expr ~s" e)])))


(define purify-letrec
  (lambda (program)
    (match program
      [,[Expr -> prog] prog]
      [,e (errorf who "Program ~s" e)])))

);; end lib
