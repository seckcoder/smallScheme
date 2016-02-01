(library 
  (Compiler Optimizations optimize-source)
  (export optimize-source)
  (import 
    (Framework match)
    (Framework helpers)
    (Compiler my-helpers)
    (chezscheme))


  ;; INPUT 
  ;;
  ;;Program 
  ;;  |Expr
  ;;Expr 
  ;;  |label
  ;;  |uvar
  ;;  |(quote Immediate)
  ;;  |(if Expr Expr Expr)
  ;;  |(begin Expr* Expr)
  ;;  |(let ([uvar Expr]*) Expr)
  ;;  |(letrec ([label (lambda (uvar*) Expr)]*) Expr)
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
  ;;  |label
  ;;  |uvar
  ;;  |(quote Immediate)
  ;;  |(if Expr Expr Expr)
  ;;  |(begin Expr* Expr)
  ;;  |(let ([uvar Expr]*) Expr)
  ;;  |(letrec ([label (lambda (uvar*) Expr)]*) Expr)
  ;;  |(prim Expr*)
  ;;  |(Expr Expr*)
  ;;immediate 
  ;;  | () 
  ;;  | #t  
  ;;  | #f



  (define who 'optimize-source)

  (define fold-constants
    (lambda (expr)
      (match expr
        [(,rator (quote ,rand1) (quote ,rand2))
         (guard (memq rator '(+ - * < > <= = >= /)) (fixnum? rand1) (fixnum? rand2))
         (let ([res (eval  `(,rator ,rand1 ,rand2))])
           (if (or (boolean? res) (fixnum-range? res)) `(quote ,res) expr))]
        [,_ _])))

  (define fold-if
    (lambda (expr)
      (match expr
        [(if (quote ,i) ,c ,a) (if i c a)]
        [,_ _])))


  ;;We draw values from our env in Expr. This determines
  ;;if we propagate an expression into a uvar, keep it the same, or mark out as dead code 
  (define extend-env
    (lambda (x a env expr)
      (match a
        [(quote ,i)  `((,x (quote ,i)) ,env ...)]
        [,x
          (let-values ([(reads writes) (propagation-ok 0 0 x expr)])
            (if (and (zero? reads) (zero? writes))
              `((,x (nop)) ,env ...) ;;nop should be fine since it has no reads or writes, who cares about the value
              (if (zero? writes)
                `((,x ,a) ,env ...)
                `((,x ,x) ,env ...))))])))

  ;;A complex expression should be able to be propagated iff there is < 1 write in the body 
  ;;of its let/rec binding (it is never affected) and N reads.
  ;;A variable with 0 reads is dead code and can be eliminated
  (define propagation-ok
    (lambda (reads writes var expr)
      (match expr
        [(cons ,datum ,ptr) (guard (eq? ptr var)) (values reads (add1 writes))]
        [(,prim ,ptr) (guard (eq? ptr var) (prim? prim)) (values(add1 reads) writes)]
        [(,prim ,ptr ,datum) (guard (eq? ptr var) (effect-prim? prim))
                             (values reads (+ writes 2))] 
        ;;add 2 to writes because effectful, so we should never propagate it
        [(,prim ,ptr ,datum1 ,datum2) (guard (eq? ptr var) (effect-prim? prim))
                                      (values reads (+ writes 2))] 
        ;;add 2 to writes because effectful, so we should never propagate it
        [(,prim ,v1 ,v2) (guard (or  (eq? v1 var) (eq? v2 var)) (value-prim? prim)) (values (add1 reads) writes)]
        [(,[ar aw] ,[dr* dw*] ...) 
         (values (fold-left + ar dr*) (fold-left + aw dw*))]
        [,x (guard (eq? x var)) (values (add1 reads) writes)]
        [,_ (values reads writes)])))

  ;; To be safe, we have to make sure that the arguments of a primop in a begin do not themselves contain side effects. 
  (define effectful?
    (lambda (e)
      (match e
        [(,prim ,x ,y) (guard (effect-prim? prim)) #t]
        [(,prim ,x ,y ,z) (guard (effect-prim? prim)) #t]
        [(,[a] ,[d*] ...) (ormap (lambda (x) x) (cons a d*))]
        [,_ #f])))

  ;;anything that isn't in tail position which isn't an effect can be optimized out
  (define remove-non-effects
    (lambda (e)
      (match e
        [(,prim ,x) (guard (or (value-prim? prim) (pred-prim? x)) (not (effectful? x))) '(nop)]
        [(,prim ,x ,y) 
         (guard (or (pred-prim? prim) (value-prim? prim)) (not (effectful? x)) (not (effectful? y))) '(nop)]
        [(quote ,i) '(nop)]
        [,_ _])))

  ;;remove the simple constant bindings in a let/rec. (they will be propagated via the env)
  (define remove-simple 
    (lambda (uvar expr*)
      (fold-left
        (lambda (s u e)
          (if (or (uvar? e) (label? e) (and (pair? e)   (not (eq? (car e) 'quote))))
            `((,u ,e) ,@s) s))
        '() uvar expr*)))

  ;;remove all bindings that are going to get eliminated in the body
  (define remove-complex
    (lambda (bindings env)
      (fold-left
        (lambda (s u e)
          (if (or (eq? (get env u) '(nop)) (not (eq? (get env u) u))) s `((,u ,e) ,@s)))
        '() (map car bindings) (map cadr bindings))))

  (define Expr
    (lambda (env) 
      (lambda (program)
        (match program
          [(false) '(false)]
          [(true) '(true)]
          [(quote ,i)  `(quote ,i)]
          [(if ,[p] ,[c] ,[a]) (fold-if `(if ,p ,c ,a))]
          [(begin ,[e*] ... ,[p]) (mmake-begin `(,(map remove-non-effects e*) ... ,p))]
          [(let ([,uvar ,[expr*]] ...) ,expr)
           (let* ([env^ (fold-left (lambda (env x a) (extend-env x a env expr)) env uvar expr*)]
                  [bindings (remove-simple uvar expr*)]
                  [bindings (remove-complex bindings env^)])
             `(let ,bindings ,((Expr env^) expr)))]
          [(letrec ([,label* (lambda ,uvar* ,[expr*])] ...) ,expr)
           `(letrec ([,label* (lambda ,uvar* ,expr*)] ...) ,((Expr env)  expr))]
          [(,prim ,v) 
           (guard (pred-prim? prim) (immediate? (get-imm ((Expr env) v)))) 
           `(quote ,(eval `(,prim ,((Expr env) v))))]
          [(,prim ,[v*] ...) (guard (prim? prim)) (fold-constants `(,prim ,v* ...))]
          [,x (guard (or (uvar? x) (label? x))) (get env x)]
          [(,[expr] ,[expr*] ...) `(,expr ,expr* ...)]
          [,e (errorf who "Expr ~s" e)]))))



  (define optimize-source
    (lambda (program)
      (match program
        [,[(Expr '()) ->  prog] prog]
        [,e (errorf who "Program ~s" e)])))

  ) ;; end lib
