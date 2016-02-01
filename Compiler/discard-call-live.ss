(library (Compiler discard-call-live)
  (export discard-call-live)
  (import
    (Framework helpers)
    (Framework match)
    (Compiler my-helpers)
    (chezscheme))
;; Nothing really to say here. We just find (,triv ,loc ...) and lop it off 
  
  
  (define discard-call-live
    (lambda (prog)
      (match prog
        [(letrec ([,labels (lambda () ,[Body -> body])] ...) ,[Body -> body*])
         `(letrec ([,labels (lambda () ,body)] ...) ,body*)])))

  (define Body
    (lambda (prog)
      (match prog
        [(locate ([,uvar ,reg] ...) ,[Tail -> tail])
         `(locate ([,uvar ,reg] ...) ,tail)])))

  (define Tail
    (lambda (tail)
      (match tail
        [(if ,[Pred -> pred] ,[tail1] ,[tail2])
         `(if ,pred ,tail1 ,tail2)]
        [(begin ,[Effect -> eff*] ... ,[tail])
         `(begin ,eff* ... ,tail)]
        [(,triv ,loc ...)
         `(,triv)]
        [,else else])))
(define Effect
  (lambda (effct)
    (match effct
      [(nop) '(nop)]
      [(set! ,lhs ,rhs) `(set! ,lhs ,rhs)]
      [(mset! ,t1 ,t2 ,t3) `(mset! ,t1 ,t2 ,t3)]
      [(return-point ,rpl ,[Tail -> tl]) `(return-point ,rpl ,tl)]
      [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
      [(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)]
      [,e (errorf 'discard-call-live "Effect ~s" e)])))

    (define Pred
      (lambda (pred)
        (match pred
          [(true)  '(true)]
          [(false) '(false)]
          [(if ,[p] ,[c] ,[a]) `(if ,p ,c ,a)]
          [(begin ,[Effect -> e*] ... ,[p]) `(begin ,e* ... ,p)]
          [(,relop ,t1 ,t2) `(,relop ,t1 ,t2)]
          [,e (error 'discard-call-live "Pred ~s" e)])))

  );; end lib
