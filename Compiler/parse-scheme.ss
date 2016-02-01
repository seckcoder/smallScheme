(library
  (Compiler parse-scheme)
  (export parse-scheme)
  (import
   (chezscheme)
   (Compiler my-helpers)
   (Framework match)
   (Framework helpers))


  (define who 'parse-scheme)

    (define prims
      '((+           . 2) (-             . 2) 
        (*           . 2) (<=            . 2)
        (<           . 2) (=             . 2)
        (>=          . 2) (>             . 2)
        (boolean?    . 1) (car           . 1) 
        (cdr         . 1) (cons          . 2) 
        (eq?         . 2) (fixnum?       . 1)
        (make-vector . 1) (null?         . 1) 
        (pair?       . 1) (procedure?    . 1) 
        (set-car!    . 2) (set-cdr!      . 2) 
        (vector?     . 1) (vector-length . 1)
        (vector-ref  . 2) (vector-set!   . 3) 
        (void        . 0)))

    (define constant?
      (lambda (x)
      (or (boolean? x) (null? x) (fixnum? x))))

    (define datum? 
      (lambda (x)
        (or (constant? x)
            (if (pair? x)
              (and (datum? (car x)) (datum? (cdr x)))
              (and (vector? x) (andmap datum? (vector->list x)))))))

    (define extend-env
      (lambda (a)
        `(,a ,(unique-name a))))

    (define convert-and
      (lambda (env args)
        (cond
          [(null? args) `(quote #t)]
          [(= (length args) 1) (car args)]
          [else 
            (if (and (pair? (car args)) (pred-prim? (caar args)))
              `(if ,(car args) ,((Expr env) `(and ,@(cdr args))) '#f)
              (let ([t (unique-name 't)])
                `(let ([,t ,(car args)])
                   (if ,t ,((Expr env) `(and ,@(cdr args))) ,t))))])))

    (define convert-or
      (lambda (env args)
        (cond
          [(null? args) `(quote #f)]
          [(= (length args) 1) (car args)]
          [else 
            (if (and (pair? (car args)) (pred-prim? (caar args)))
              `(if ,(car args) '#t ,((Expr env) `(or ,@(cdr args))))
              (let ([t (unique-name 't)])
                `(let ([,t ,(car args)])
                   (if ,t ,t ,((Expr env) `(or ,@(cdr args)))))))])))

    (define not-in-env
      (lambda (env keyword)
        (not (uvar? (get env keyword)))))


    (define Expr
      (lambda (env)
        (lambda (x)
          (match x
            [(or ,[a*] ...)  (guard (not-in-env env 'or))  (convert-or env a*)]
            [(and ,[a*] ...) (guard (not-in-env env 'and)) (convert-and env a*)]
            [(not ,[e]) (guard (not-in-env env 'not)) `(if ,e '#f '#t)]
            [(quote ,x) (guard (not-in-env env 'quote))
                        (unless (datum? x) (errorf who "invalid datum ~s\n" x)) `(quote ,x) ]
            [(if ,[p] ,[c] ,[a]) (guard (not-in-env env 'if)) `(if ,p ,c ,a)]
            [(if ,[p] ,[c])      (guard (not-in-env env 'if)) `(if ,p ,c (void))]
            [(begin ,[e*] ... ,[e])     (guard (not-in-env env 'begin))`(begin ,e* ... ,e)]
            [(lambda (,fml* ...) ,x ,x* ...) 
             (guard (not-in-env env 'lambda))
             (unless (set? fml*)
               (errorf who "duplicate variable in formals list ~s\n" fml*))
             (unless  (andmap symbol? fml*)
               (errorf who "invalid item in formals list ~s\n" fml*))
             (let* ([env^ (map extend-env fml*)]
                    [envr-extend (append env^ env)])
               `(lambda (,(map cadr env^) ...)
                  ,(make-begin `(,((Expr envr-extend) x) ,(map (Expr envr-extend) x*) ...))))]
            [(let ([,new-env ,[e*]] ...) ,x ,x* ...) 
             (guard (not-in-env env 'let))
             (unless (set? new-env)
               (errorf who "duplicate variable in parameter list ~s\n" new-env))
             (unless  (andmap symbol? new-env)
               (errorf who "invalid item in parameter list ~s\n" new-env))
             (let* ([env^ (map extend-env new-env)]
                    [envr-extend (append env^ env)])
               `(let (,(map (lambda (u e) `(,u ,e))  (map cadr env^) e*) ...)
                  ,(make-begin `(,((Expr envr-extend) x) ,(map (Expr envr-extend) x*) ...))))]
            [(letrec ([,new-env ,rhs*] ...) ,x ,x* ...) 
             (guard (not-in-env env 'letrec))
             (unless (set? new-env)
               (errorf who "duplicate variable in parameter list ~s\n" new-env))
             (unless  (andmap symbol? new-env)
               (errorf who "invalid item in parameter list ~s\n" new-env))
             (let* ([env^ (map extend-env new-env)]
                    [p (Expr (append env^ env))])
               `(letrec (,(map (lambda (u e) `(,u ,e))  (map cadr env^) (map p rhs*))...)
                  ,(make-begin `(,(p x) ,(map p x*) ...))))]
            [(set! ,var ,[rhs]) (guard (not-in-env env 'set!))
                                (unless (symbol? var) (errorf who "invalid set! lhs ~s" var))
                                (if (uvar? (get env var)) 
                                  `(set! ,(get env var) ,rhs)
                                  (errorf who "unbound uvar ~s" var))]
            [(,prim ,x* ...) (guard (assq prim prims) (not-in-env env prim))
                             (unless (= (length x*) (cdr (assq prim prims)))
                               (errorf who "too many or few arguments ~s for ~s" (length x*) prim))
                             `(,prim ,(map (Expr env) x*) ...)]
            [(,x ,y ...) (guard (not (or (symbol? x) (pair? x)))) (errorf who "invalid Expr ~s" `(,x ,y ...))]
            [(,[p] ,[a*] ...) `(,p ,a* ...)]
            [,var (guard (symbol? var) (not (uvar? var)))
                  (if (eq? var (get env var))
                    (errorf who "unbound variable ~s" var)
                    (get env var))]
            [,constant  (guard (datum? constant)) `(quote ,constant)]
            [,uvar (guard (uvar? uvar)) (get env uvar)]
            [,x (errorf who "invalid Expr ~s" x)]))))

    (define parse-scheme
      (lambda (x) ((Expr '()) x)))

    );; end lib
