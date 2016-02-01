(library
  (Compiler convert-assignments)
  (export convert-assignments)
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

(define who 'convert-assignments)

(define convert-set!
  (lambda (mutants expr)
    (define  convert
      (lambda (e)
        (cond
          [(null? e) '()]
          [(atom? e) (if (member? e mutants) `(car ,e) e)]
          [(pair? (car e)) (cons (convert (car e)) (convert (cdr e)))]
          [(or (eq? (car e) 'set!) (eq? (car e) 'set-car!)) 
            `(set-car! ,(cadr e) ,(convert (caddr e)))]
          [(member? (car e) mutants) (cons `(car ,(car e)) (convert (cdr e)))]
          [else (cons (car e) (convert (cdr e)))])))
    (let* ([binds (fold-left
                    (lambda (s uvar)
                      (let ([tmp (unique-name 'x)])
                        `(,(cons `(,uvar ,tmp) (car s)) . ,(cons `(,uvar (cons ,tmp (void))) (cdr s)))))
                    '(() . ())
                    mutants)]
           [env (car binds)]
           [assigns (cdr binds)]
           [sets (convert expr)])
      (values env `(let ,assigns ,sets)))))

;;remove extranious lets and letrecs. should not be run for efficiency, as the code is semantically equivalent
(define clean-let/letrec
  (lambda (prog)
    (match prog
      [(let () ,[expr]) expr]
      [(letrec () ,[expr]) expr]
      [(,[expr] ,[expr*] ...) `(,expr ,expr* ...)]
      [,_ _])))

(define Expr
  (lambda (program)
    (match program
      [(false) '(false)]
      [(true)  '(true)]
      [(quote ,i) (guard (immediate? i)) `(quote ,i)]
      [(if ,[p] ,[c] ,[a]) `(if ,p ,c ,a)]
      [(begin ,[e*] ... ,[p]) (make-begin `(,e* ... ,p))]
      [(let ([,uvar* ,[expr*]] ...) (assigned ,asgns ,[expr]))
       (let*-values ([(env expr^) (convert-set! asgns expr)]
                     [(binds) (map (lambda (u e) `(,(get env u) ,e)) uvar* expr*)])
                    `(let ,binds  ,expr^))]
      [(letrec ([,uvar* ,[expr*]] ...) (assigned ,asgns ,[expr]))
       (let*-values ([(env expr^) (convert-set! asgns expr)]
                     [(binds) (map (lambda (u e) `(,(get env u) ,e)) uvar* expr*)])
                    `(letrec ,binds ,expr^))]
      [(letrec ([,uvar ,[expr*]] ...) ,[expr])
       `(letrec ([,uvar ,expr*] ...)  ,expr)]
      [(let ([,uvar* ,[expr*]] ...) ,[expr])
       `(let ([,uvar* ,expr*] ...)  ,expr)]
      [(lambda (,uvar* ...) (assigned ,asgns ,[expr]))
       (let*-values ([(env expr^) (convert-set! asgns expr)]
                     [(binds) (map (lambda (u) (get env u)) uvar*)])
                    `(lambda ,binds ,expr^))]
      [(set! ,uvar ,[expr]) `(set! ,uvar ,expr)]
      [(,prim ,[v*] ...) (guard (prim? prim)) `(,prim ,v* ...)]
      [,x (guard (or (uvar? x) (label? x) (immediate? x))) x]
      [(,[expr] ,[expr*] ...) `(,expr ,expr* ...)]
      [,e (errorf who "Expr ~s" e)])))

(define-who  convert-assignments

  (lambda (program)
    (match program
      [,[Expr -> prog] (clean-let/letrec prog)]
      [,e (errorf who "Program ~s" e)]))
  )
)
