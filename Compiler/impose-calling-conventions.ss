(library 
  (Compiler impose-calling-conventions)
  (export impose-calling-conventions)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers)
    (Compiler my-helpers) )

  (define who 'impose-calling-conventions)

  (define rp (unique-name 'rp))

  ;; Map until one of the lists is null, and then stop
  (define uneven-map
    (lambda (proc ls1 ls2)
      (cond
        [(or (null? ls1) (null? ls2)) '()]
        [else (cons (proc (car ls1) (car ls2)) (uneven-map proc (cdr ls1) (cdr ls2)))])))

  (define impose-calling-conventions
    (lambda (prog)
      (match prog
        [(letrec ([,label* (lambda ,uvar* ,body*)] ...) ,body)
         `(letrec ,(map (lambda (l u b) `(,l (lambda () ,((Body u (unique-name 'rp)) b))))
                        label* uvar* body*) 
            ,((Body '() (unique-name 'rp)) body))]
        [,e (errorf who "Error with ~s" e)])))


  (define Body
    (curry ((formals rp) body)

      (define frames '())
      (define fvs)
      (define update!
        (lambda (new-fvs)
          (set! frames (cons new-fvs frames))))

      (define Tail
        (curry ((formals rp) tail)
          (match tail
            [(alloc ,t)
             (make-begin
               `((set! ,return-value-register (alloc ,t))
                 (,rp ,frame-pointer-register 
                      ,allocation-pointer-register 
                      ,return-value-register)))]
            [(mref ,t1 ,t2)
             (make-begin
               `((set! ,return-value-register (mref ,t1 ,t2))
                 (,rp ,frame-pointer-register 
                      ,allocation-pointer-register 
                      ,return-value-register)))]
            [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
            [(begin ,[Effect -> e*] ... ,[t]) (make-begin `(,e* ... ,t))]
            [(,binop ,t1 ,t2) 
             (guard (binop? binop))
             (make-begin 
               `((set! ,return-value-register (,binop ,t1 ,t2))
                 (,rp ,frame-pointer-register ,allocation-pointer-register ,return-value-register)))]
            [(,triv ,triv* ...) 
             (let ((locs (create-locs triv* parameter-registers 0 index->frame-var #f)))
               (make-begin 
                 `((set! ,return-address-register ,rp) 
                   ,locs ...
                   (,triv ,return-address-register ,allocation-pointer-register ,frame-pointer-register 
                          ,(map cadr locs) ...))))]
            [,triv (guard (triv? triv)) (make-begin `((set! ,return-value-register ,triv)
                                                      (,rp ,frame-pointer-register 
                                                           ,allocation-pointer-register 
                                                           ,return-value-register)))]
            [,e (errorf who "Tail ~s" e)])))

      (define Pred 
        (lambda (prd)
          (match prd
            [(false) '(false)]
            [(true)  '(true)]
            [(if ,[p1] ,[p2] ,[p3]) `(if ,p1 ,p2 ,p3)]
            [(begin ,[Effect -> e*] ... ,[p]) (make-begin `(,e* ... ,p))]
            [(,relop ,v1 ,v2) `(,relop ,v1 ,v2)]
            [,e (errorf who "Pred ~s" e)])))

      (define Effect 
        (lambda (effct)
          (match effct
            [(nop) '(nop) ]
            [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
            [(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)]
            [(set! ,uvar (alloc ,t)) `(set! ,uvar (alloc ,t))]
            [(set! ,uvar (mref ,t1 ,t2)) `(set! ,uvar (mref ,t1 ,t2))]
            [(set! ,uvar (,binop ,t1 ,t2)) (guard (binop? binop)) `(set! ,uvar (,binop ,t1 ,t2))]
            [(set! ,uvar (,proc ,triv ...))
             (make-begin
               `(,(Effect `(,proc ,triv ...)) (set! ,uvar ,return-value-register)))]
            [(set! ,uvar ,triv) `(set! ,uvar ,triv)]
            [(mset! ,t1 ,t2 ,t3) `(mset! ,t1 ,t2 ,t3)]
            [(,proc ,triv* ...) 
             (let* ([rp-lab (unique-label 'rp)]
                    [regs (uneven-map (lambda (x y) x) parameter-registers triv*)]
                    [locs (create-locs triv* regs 0 (lambda (v) (unique-name 'newfv)) #f)])
               (update! (filter uvar? (map cadr locs)))
               `(return-point ,rp-lab
                              ,(make-begin 
                                 `((set! ,return-address-register ,rp-lab)
                                   ,locs ... 
                                   (,proc ,return-address-register ,frame-pointer-register 
                                          ,(map cadr locs) ...)))))]
            [,e (errorf who "Effect ~s" e)])))


      (match body
        [(locals ,uvar* ,[(Tail formals rp) -> tail]) 
         `(locals (,rp ,uvar* ... ,formals ... ,frames ... ...)
            (new-frames ,frames 
              ,(make-begin `(,(make-inits formals rp) ... ,tail))))]
        [,e (errorf who "Body ~s" e)])))


  ;; flag = #f: reverses fvar and triv. So  (set! fvar triv))
  ;; flag = #t:  (set! triv fvar) (for make-inits)
  (define-with-implicits create-locs (flag) 
                         (lambda (formals regs n proc)
                           (cond
                             [(null? formals) '()]
                             [(null? regs) ;; Weve run out of regs, so start assigining frames
                              (let ((lhs (if flag (car formals) (proc n)))
                                    (rhs (if flag (proc n) (car formals))))
                                (cons `(set! ,lhs ,rhs)
                                      (create-locs (cdr formals) regs (add1 n) proc)))]
                             [else ;; else keep assigining to registers
                               (let ((lhs (if flag (car formals) (car regs)))
                                     (rhs (if flag (car regs) (car formals))))
                                 (cons `(set! ,lhs ,rhs)
                                       (create-locs (cdr formals) (cdr regs)  n proc)))])))

  (define make-inits 
    (lambda (formals return-ptr)
      `((set! ,return-ptr ,return-address-register) 
        . ,(create-locs formals parameter-registers 0 index->frame-var #t))))

  (define binop?
    (lambda (x)
      (memq x '(+ - * logand logor sra))))

  (define triv?
    (lambda (x)
      (ort x label? uvar? int64?))) 

  );end library

