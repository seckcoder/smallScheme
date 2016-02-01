(library 
  (Compiler Optimizations optimize-self-reference)
  (export optimize-self-reference)
  (import 
    (Framework match)
    (Framework helpers)
    (Compiler my-helpers)
    (chezscheme))

  (define who 'optimize-self-reference)

  ;; find self-referential functions
  (define find-self-refs
    (lambda (element)
      (let ([f-name (car element)] [f-params (cddr element)])
        (if (memq f-name f-params)
          `(,(unique-label f-name) . ,f-name) 
          '()))))

  (define uv->fp
    (case-lambda 
      [(param* uvar env)
      (cond
        [(null? param*) '()]
        [else (let* ([first (car param*)]
                     [present (assq first env)]
                     [done (uv->fp (cdr param*) uvar env)])
                (if present
                  (cons (caddr present) done)
                  (if (eq? uvar first) 
                    done
                    (cons first done))))])]
      [(expr env)
       (cond 
         [(null? expr) '()]
         [(uvar? (car expr)) 
               (let* ([first (car expr)]
                      [present (assq first env)]
                      [done (uv->fp (cdr expr) env)])
                 (if present 
                   (cons (caddr present) done)
                   (cons first done)))]
         [else (cons (car expr) (uv->fp (cdr expr) env))])] ))

  ;; If a uvar is self-refered, then replace it with its pointer
  (define sanitize-closures
    (lambda (closures env)
      (lambda (closure)
        (match closure
          [(,uvar ,label ,param* ...)
           (if (null? (intersection param* (map cdr closures)))
             `(,uvar ,label ,param* ...)
             (let ([params (uv->fp param* uvar env)])
               `(,uvar ,label ,params ...)))]
          [,e (errorf who "sanitize-closures ~s" e)]))))

  (define Expr
    (lambda (closures env)
      (lambda (expr)
        (match expr
          [(if ,[test] ,[conseq] ,[alt]) `(if ,test ,conseq ,alt)]
          [(quote ,im) `(quote ,im)]
          [(begin ,[expr*] ... ,[tail]) `(begin ,expr* ... ,tail)]
          [(letrec (,[(Body closures '())-> body*] ...)
             (closures (,[(sanitize-closures closures env) -> clos*] ...) ,[tail]))
           `(letrec (,body* ...) (closures (,clos* ...) ,tail))]
          [(let ([,uvar* ,[expr*]] ...) ,[tail]) `(let ([,uvar* ,expr*] ...) ,tail)]
          [(,prim ,[expr*] ...) (guard (prim? prim))  `(,prim ,expr* ...)]
          [(,[expr-a] ,[expr] ,[rem*] ...)
           `(,(uv->fp `(,expr-a ,expr ,rem* ...) env) ... )]
          [(,[expr-a] ,[expr] ,[rem*] ...)
           `(,expr-a ,(uv->fp `(,expr ,rem* ...) env) ...)]   
          [,x (guard (or (uvar? x) (label? x))) x]
          [,e (errorf who "Expr ~s" e)]))))

  ;; Get each label and corresponding lambda expressions
  (define Body
    (lambda (closures env)
      (lambda (body)
        (match body
          [(,label (lambda (,fptr ,param* ...) (bind-free (,free* ...) ,tail)))
           (let ([present? (assq label closures)]) ;; self-referent?
             ;; If so, uppdate env and record pointer
             (if present?
               (let* ([uvar (cdr present?)]
                      [new-env (cons `(,uvar . (,label ,fptr)) env)])
                 `(,label (lambda (,fptr ,param* ...)
                            (bind-free (,(remq uvar free*) ...)
                                       ,((Expr closures new-env) tail)))))
               ;; else, just keep going
               `(,label (lambda (,fptr ,param* ...) (bind-free (,free* ...)
                                     ,((Expr closures env) tail))))))]
          [,e (errorf who "Body ~s" e)]))))

  (define gather-closures
    (lambda (expr)
      (match expr
        [(if ,[test] ,[conseq] ,[alt]) `(,test ... ,conseq ... ,alt ...)]
        [(quote ,x) '()]
        [(begin ,[expr*] ... ,[tail]) `(,expr* ... ... ,tail ...)]
        [(letrec ([,label* (lambda (,param* ...) (bind-free (,fptr ,free* ...) ,[body*]))] ...) 
           (closures (,func* ...) ,[tail])) 
         `(,body* ... ... ,func* ... ,tail ...)]
        [(let ([,uvar* ,[exp*]] ...) ,[tail]) `(,exp* ... ... ,tail ...)]
        [(,prim ,[expr*] ...) (guard (prim? prim)) `(,expr* ... ...)]
        [(,[expr-a] ,[expr] ,[rem*] ...) `(,expr-a ... ,expr ... ,rem* ... ...)]
        [,x (guard (or (uvar? x) (label? x))) '()]
        [,e (errorf who "gather-closures ~s" e)])))


  (define optimize-self-reference
    (lambda (x)
      (let* ([closure-list (gather-closures x)]
             [self-referent-closures (remq '() (map find-self-refs closure-list))]) 
        ;; should be faster to call remq then do apply append (hence whi we do this)
        ((Expr self-referent-closures '()) x))))

  );; end lib
