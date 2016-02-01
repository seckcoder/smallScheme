(library
  (Compiler normalize-context)
  (export normalize-context)
  (import
   (chezscheme)
   (Framework helpers)
   (Framework match)
   (Compiler my-helpers))


;;  INPUT
;;
;;Program
;;  |(letrec ([label (lambda (uvar*) Expr)]*) Expr)
;;Expr
;;  |Expr 
;;  |label
;;  |uvar
;;  |(quote Immediate)
;;  |(if Expr Expr Expr)
;;  |(begin Expr* Expr)
;;  |(let ([uvar Expr]*) Expr)
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
;;  |(letrec ([label (lambda (uvar*) Value)]*) Value)
;;Value 
;;  |label
;;  |uvar
;;  |(quote Immediate)
;;  |(if Pred Value Value)
;;  |(begin Effect* Value)
;;  |(let ([uvar Value]*) Value)
;;  |(value-prim Value*)
;;  |(Value Value*)
;;Pred
;;  |(true)
;;  |(false)
;;  |(if Pred Pred Pred)
;;  |(begin Effect* Pred)
;;  |(let ([uvar Value]*) Pred)
;;  |(pred-prim Value*)
;;Effect
;;  |(nop)
;;  |(if Pred Effect Effect)
;;  |(begin Effect* Effect)
;;  |(let ([uvar Value]*) Effect)
;;  |(effect-prim Value*)
;;  |(Value Value*)
;;Immediate 
;;  |fixnum 
;;  | () 
;;  | #t 
;;  | #f

(define who 'normalize-context)

    (define Value
      (lambda (value)
        (match value
          [(let ([,uvar* ,[v*]] ...) ,[v])
           `(let ([,uvar* ,v*] ...) ,v)]
          [(begin ,[Effect -> e*] ... ,[v])
           (mmake-begin `(,e* ... ,v))]
          [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
          [(quote ,i) (guard (immediatefixnum? i)) `(quote ,i)]
          [(,prim ,[v*] ...) (guard (value-prim? prim))
           `(,prim ,v* ...)]
          [(,v ,v* ...)
           (guard (or (pred-prim? v) (effect-prim? v)))
           (Expr->Value `(,v ,v* ...))]
          [,x (guard (or (label? x) (uvar? x))) x]
          [(,[v] ,[v*] ...)
           `(,v ,v* ...)]
          [,e (errorf who "Value ~s" e)])))

    (define Effect
      (lambda (effect)
        (match effect
          [(nop) '(nop)]
          [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
          [(begin ,[e*] ... ,[e])
           (mmake-begin `(,e* ... ,e))]
          [(let ([,uvar* ,[Value -> v*]] ...) ,[e])
           `(let ([,uvar* ,v*] ...) ,e)]
          [(,prim ,[Value -> v*] ...) (guard (effect-prim? prim))
           `(,prim ,v* ...)]
          [(,v ,v* ...)
           (guard (not (pred-prim? v)) (not (value-prim? v)) (not (eq? v 'quote)))
           `(,(Value v) ,(Value v*) ...)]
          [,expr (Expr->Effect expr)])))

    (define Pred
      (lambda (pred)
        (match pred
          [(true) '(true)]
          [(false) '(false)]
          [(if ,[p] ,[c] ,[a]) `(if ,p ,c ,a)]
          [(begin ,[Effect -> e*] ... ,[p])
           (mmake-begin `(,e* ... ,p))]
          [(let ([,uvar* ,[Value -> v*]] ...) ,[e])
           `(let ([,uvar* ,v*] ...) ,e)]
          [(,prim ,[Value -> v*] ...) (guard (pred-prim? prim)) `(,prim ,v* ...)]
          [,expr (Expr->Pred expr)])))

    (define Expr->Pred
      (lambda (expr)
        (match expr
          [(,prim ,[Value -> arg*] ...) (guard (effect-prim? prim))
           (mmake-begin `((,prim ,arg* ...) (true)))]
          [(quote ,i) (guard (eq? i '#t)) '(true)]
          [(quote ,i) (guard (eq? i '#f)) '(false)]
          [(,prim ,[Value ->  arg*] ...) (guard (value-prim? prim))  `(if (eq? (,prim ,arg* ...) '#f) (false) (true))]
          [(quote ,v) `(if (eq? (quote ,v)  '#f) (false) (true))]
          [(,[Value -> prim] ,[Value ->  arg*] ...)  `(if (eq? (,prim ,arg* ...) '#f) (false) (true))]
          [,x (guard (or (uvar? x) (label? x)))  `(if (eq? ,x '#f) (false) (true))]
          [,e (errorf who "Expr->Pred ~s" e)])))

    (define Expr->Effect
      (lambda (expr)
        (match expr
          [(,prim ,[Effect -> arg*] ...)
           (mmake-begin  `(,arg* ...))]
          [,x '(nop)]
          [,e (errorf who "Expr->Effect ~s" e)])))

    (define Expr->Value
      (lambda (expr)
        (match expr
          [(,prim ,[Value -> v*] ...) (guard (pred-prim? prim))
           `(if (,prim ,v* ...) '#t '#f)]
          [(,prim ,[Value ->  v*] ...) (guard (effect-prim? prim))
           (mmake-begin `((,prim ,v* ...) (void)))]
          [,e (errorf who "Expr->Value ~s" e)])))

  (define normalize-context
    (lambda (program)
      (match program
        [(letrec ([,label* (lambda ,uvar* ,[Value ->  expr*])] ...) ,[Value -> expr])
         `(letrec ([,label* (lambda ,uvar* ,expr*)] ...) ,expr)]
        [,e (errorf who "Prog ~s" e)])))

  );end library


