(library 
  (Compiler flatten-set!)
  (export flatten-set!)
  (import 
    (Compiler my-helpers)
    (Framework match)
    (Framework helpers)
    (chezscheme))

  (define who 'flatten-set!)

  (define flatten-set!
    (lambda (prog)
      (match prog
        [(letrec ([,lbl* (lambda (,uvars ...) 
                            ,[Body -> bd*])] ...) ,[Body -> bd])
         `(letrec ([,lbl* (lambda (,uvars ...) ,bd*)] ...) ,bd)]
        [,e (errorf who "Error with ~s" e)])))

  (define Body
    (lambda (b)
      (match b
        [(locals ,uvar* ,[Tail -> tl]) `(locals ,uvar* ,tl)]
        [,e (errorf who "Body  ~s" e)])))

  (define Effect
    (lambda (effct)
      (match effct
        [(nop) '(nop)]
        [(if ,[Pred -> p] ,[c] ,[a])
         `(if ,p ,c ,a)]
        [(begin ,[eff*] ... ,[eff])
         (make-begin `(,eff* ... ,eff))]
        [(set! ,uvar (begin ,[eff*] ... ,[Value -> val]))
         (make-begin `(,eff* ... ,(Effect `(set! ,uvar ,val))))]
        [(set! ,uvar (if ,[Pred -> p] ,[Value -> c] ,[Value -> a]))
         `(if ,p ,(Effect `(set! ,uvar ,c)) ,(Effect `(set! ,uvar ,a)))]
        [(set! ,uvar ,[Value -> v]) `(set! ,uvar ,v)]
        [(,t ,t* ...) `(,t ,t* ...)]
        [,e (errorf who "Effect  ~s" e)])))

  (define Value
    (lambda (vl)
      (match vl
        [(if ,[Pred -> p] ,[c] ,[a])
         `(if ,p ,c ,a)]
        [(begin ,[Effect -> eff*] ... ,[val])
         (make-begin `(,eff* ... ,val))]
        [(,op ,t1 ,t2) (guard (binop? op)) 
                       `(,op ,t1 ,t2)]
        [,triv (guard (triv? triv)) triv] 
        [(,t ,t* ...) `(,t ,t* ...)]
        [,e (errorf who "Value ~s" e)])))

  (define Pred
    (lambda (prd)
      (match prd
        [(true) '(true)]
        [(false) '(false)]
        [(if ,[p] ,[c] ,[a])
         `(if ,p ,c ,a)]
        [(begin ,[Effect -> eff*] ... ,[p])
         (make-begin `(,eff* ... ,p))]
        [(,op ,t1 ,t2) `(,op ,t1 ,t2)]
        [,e (errorf who "Pred ~s " e)])))

  (define Tail
    (lambda (tl)
      (match tl
        [(begin ,[Effect -> eff*] ... ,[tl])
         (make-begin `(,eff* ... ,tl))]
        [(if ,[Pred -> p] ,[c] ,[a])
         `(if ,p ,c ,a)]
        [,triv (guard (triv? triv)) triv]
        [(,e1 ,e2 ...) `(,e1 ,e2 ...)]
        [,e (errorf who "Tail ~s" e)])))

  (define binop?
    (lambda (x)
      (memq x '(+ - * logand logor sra))))
  
  (define triv?
    (lambda (x)
    (ort x label? uvar? int64?))) 
  )
