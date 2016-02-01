(library
  (Compiler specify-representation)
  (export specify-representation)
  (import
    (chezscheme)
    (Framework helpers)
    (Compiler my-helpers)
    (Framework match))

  (define heap-alloc?
    (lambda (x)
      (and  (memq x '(car cdr cons procedure-code make-procedure 
                          procedure-ref make-vector vector-length 
                          vector-ref set-car! set-cdr!)) #t)))
  (define who 'specify-representation)

  ;;convenience globals
  (define offset-car (- disp-car tag-pair))
  (define offset-cdr (- disp-cdr tag-pair))
  (define offset-vector-data (- disp-vector-data tag-vector))
  (define offset-procedure-data (- disp-procedure-data tag-procedure))
  (define offset-procedure-code (- disp-procedure-code  tag-procedure))
  (define offset-vector-length (- disp-vector-length tag-vector))

  ;;specify-* handles all multiplication.
  ;;its pretty self explanitory, but the only tricky thing
  ;;is the ash for the first case. See the assignment description    
  (define specify-*
    (lambda (expr)
      (match expr
        [(* (quote ,n1) (quote ,n2)) `(* ,(ash n2 word-shift) ,n1)]
        [(* ,[Value ->  uvar] (quote ,n)) `(* ,uvar ,n)]
        [(* (quote ,n) ,[Value ->  uvar]) `(* ,n ,uvar)]
        [(* ,[Value -> uvar1] ,[Value ->  uvar2]) `(* ,uvar1 (sra ,uvar2 ,word-shift))])))

  ;;specify-non-immediates will convert non immediate (heap allocated structures)
  ;;into a representation that can be handled by the UIL
  (define specify-non-immediates
    (lambda (obj)
      (match obj
        [(set-cdr! ,[Value -> ptr] ,[Value -> newptr]) `(mset! ,ptr ,offset-cdr ,newptr)]
        [(set-car! ,[Value -> ptr] ,[Value -> newptr]) `(mset! ,ptr ,offset-car ,newptr)]
        [(cdr ,[Value -> ptr]) `(mref ,ptr ,offset-cdr)]
        [(car ,[Value -> ptr]) `(mref ,ptr ,offset-car)]
        [(cons ,[Value -> item] ,[Value -> ptr])
         (let ([tmp-car (unique-name 't)] [tmp-cdr (unique-name 't)])
           (let ([tmp (unique-name 't)])
             `(let ([,tmp-car ,item] [,tmp-cdr ,ptr])
                (let ([,tmp (+ (alloc ,size-pair) ,tag-pair)])
                  ,(make-begin `((mset! ,tmp ,offset-car ,tmp-car)
                                 (mset! ,tmp ,offset-cdr ,tmp-cdr)
                                 ,tmp))))))]
        [(vector-set! ,[Value -> ptr] (quote ,[Immediate -> index]) ,[Value -> what])
         `(mset! ,ptr ,(+ offset-vector-data index) ,what)]
        [(vector-set! ,[Value -> ptr] ,[Value -> v] ,[Value -> what])
         `(mset! ,ptr (+ ,offset-vector-data ,v) ,what)]
        [(vector-length ,[Value -> ptr]) `(mref ,ptr ,offset-vector-length)]
        [(vector-ref ,[Value -> ptr] (quote ,[Immediate -> index])) `(mref ,ptr ,(+ offset-vector-data index))]
        [(vector-ref ,[Value -> ptr] ,[Value -> v]) `(mref ,ptr (+ ,offset-vector-data ,v))]
        [(make-vector  (quote ,[Immediate -> k]))
         (let ([tmp (unique-name 't)])
           `(let ([,tmp (+ (alloc ,(+ disp-vector-data k)) ,tag-vector)])
              ,(make-begin `((mset! ,tmp ,offset-vector-length ,k) ,tmp))))]
        [(make-vector ,[Value -> v])
         (let ([tmp1 (unique-name 't)] [tmp2 (unique-name 't)])
           `(let ([,tmp1 ,v])
              (let ([,tmp2 (+ (alloc (+ ,disp-vector-data ,tmp1)) ,tag-vector)])
                ,(make-begin `((mset! ,tmp2 ,offset-vector-length  ,tmp1) ,tmp2)))))]
        [(procedure-set! ,[Value -> ptr] (quote ,[Immediate -> index]) ,[Value -> what])
         `(mset! ,ptr ,(+ offset-procedure-data index) ,what)]
        [(procedure-code ,[Value -> ptr]) `(mref ,ptr ,offset-procedure-code)]
        [(procedure-ref ,[Value -> ptr] (quote ,[Immediate -> index])) `(mref ,ptr ,(+ offset-procedure-data index))]
        [(make-procedure  ,label (quote ,[Immediate -> k]))
         (let ([tmp (unique-name 't)])
           `(let ([,tmp (+ (alloc ,(+ disp-procedure-data k)) ,tag-procedure)])
              ,(make-begin `((mset! ,tmp ,offset-procedure-code ,label) ,tmp))))]
        [,e (errorf who "Non-Immediate ~s" e)])))

  ;;specify-equivalence-preds handles all of the equivalence predicates.
  ;;now with the ptr representation everything
  ;;is basically an integer comparison. 
  (define specify-equivalence-preds
    (lambda (pred)
      (match pred
        [(boolean? ,[Value -> x]) `(= (logand ,x ,mask-boolean) ,tag-boolean)]
        [(pair? ,[Value -> x]) `(= (logand ,x ,mask-pair) ,tag-pair)]
        [(fixnum? ,[Value -> x]) `(= (logand ,x ,mask-fixnum) ,tag-fixnum)]
        [(vector? ,[Value -> x]) `(= (logand ,x ,mask-vector) ,tag-vector)]
        [(procedure? ,[Value -> x]) `(= (logand ,x ,mask-procedure) ,tag-procedure)]
        [(null? ,[Value -> x]) `(= ,x ,(Immediate '()))]
        [(eq? ,[Value -> n1] ,[Value -> n2]) `(= ,n1 ,n2)]
        [(,pred (quote ,[Immediate -> n1]) (quote ,[Immediate -> n2])) `(,pred ,n1 ,n2)]
        [(,pred ,[Value -> n1] (quote ,[Immediate -> n2])) `(,pred ,n1 ,n2)]
        [(,pred (quote ,[Immediate -> n1]) ,n2) `(,pred ,n1 ,n2)]
        [(,pred ,[Value -> n1] ,[Value -> n2]) `(,pred ,n1 ,n2)]
        [,e (errorf who "Equivalence-Pred ~s" e)])))

  (define Value
    (lambda (value)
      (match value
        [(if ,[Pred -> p] ,[v1] ,[v2]) `(if ,p ,v1 ,v2)]
        [(begin ,[Effect -> e*] ... ,[val]) (make-begin `(,e* ... ,val))]
        [(quote ,[Immediate -> immediate]) immediate]
        [(let ([,binding ,[expr]] ...) ,[v]) `(let ([,binding ,expr] ...) ,v)]
        [(* ,x ,y) (specify-* `(* ,x ,y))]
        [(,vprim ,v* ...) (guard (heap-alloc? vprim)) (specify-non-immediates `(,vprim ,v* ...))]
        [(,vprim) (guard (value-prim? vprim)) (Immediate vprim)]
        [((,val ...) ,[val*] ...)
         `(,(Value val) ,val* ...)]
        [(,val ,[val*] ...)  `(,val ,val* ...)]
        [,x (guard (or (uvar? x) (label? x))) x]
        [,e (errorf who "Value ~s" e)])))

  (define Pred
    (lambda (pred)
      (match pred
        [(false) `(false)]
        [(true) `(true)]
        [(if ,[p1] ,[p2] ,[p3]) `(if ,p1 , p2 ,p3)]
        [(begin ,[Effect ->  e*] ... ,[p]) (make-begin `(,e* ... ,p))]
        [(let ([,binding ,[Value -> expr]] ...) ,[p]) `(let ([,binding ,expr] ...) ,p)]
        [(,pred-prim ,v* ...) (guard (pred-prim? pred-prim))
                              (specify-equivalence-preds `(,pred-prim ,v* ...))]
        [,e (errorf who "Pred ~s" e)])))

  (define Effect
    (lambda (effect)
      (match effect
        [(nop) '(nop)]
        [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
        [(begin ,[e*] ... ,[e]) (make-begin `(,e* ... ,e))]
        [(let ([,binding ,[Value -> expr]] ...) ,[e]) `(let ([,binding ,expr] ...) ,e)]
        [(,effect-prim ,v* ...) (guard (effect-prim? effect-prim)) (specify-non-immediates `(,effect-prim ,v* ...))]
        [((,val ...) ,[Value -> val*] ...)
         `(,(Value val) ,val* ...)]
        [(,v ,[Value -> v*] ...) `(,v ,v* ...)]
        [,e (errorf who "Effect ~s" e)])))

  (define Immediate
    (lambda (immediate)
      (match immediate
        [()   $nil]
        [#t   $true]
        [#f   $false]
        [void $void]
        [,fixnum (guard (fixnum? fixnum)) (ash fixnum shift-fixnum)]
        [(quote ,[Immediate -> x]) x]
        [,e (errorf who "Immediate ~s" e)])))



  (define specify-representation
    (lambda (prog)
      (match prog
        [(letrec ([,label* (lambda ,uvar* ,[Value -> value*])] ...) ,[Value -> value])
         `(letrec ([,label* (lambda ,uvar* ,value*)] ...) ,value)]
        [,e (errorf who "Program ~s" e)])))
  );end library
