(library 
  (Compiler remove-let)
  (export remove-let)
  (import
    (Compiler my-helpers)
    (Framework helpers)
    (Framework match)
    (chezscheme))
  
  
  (define who 'remove-let)


  (define Value
    (lambda (val)
      (match val
        [(let ([,bind* ,[ret*]] ...) ,[v])
         (make-begin `((set! ,bind* ,ret*) ... ,v))]
        [(if ,[Pred -> uvp] ,[uvc] ,[uva]) 
         `(if ,uvp ,uvc ,uva)]
        [(begin ,[Effect -> uve*] ... ,[uvv]) 
         `(begin ,uve* ... ,uvv)]
        [(sra ,[uvv] ,y) `(sra ,uvv ,y)]
        [(,binop ,[uv1] ,[uv2])
         `(,binop ,uv1 ,uv2)]
        [(alloc ,[uv]) `(alloc ,uv)]
        [(,[uv1] ,[uv*] ...) 
         `(,uv1  ,uv* ...)]
        [,e e])))

  (define Effect
    (lambda (ef)
      (match ef
        [(nop) '(nop)]
        [(let ([,bind* ,[Value -> ret*]] ...) ,[ef])
         (make-begin `((set! ,bind* ,ret*) ... ,ef))]
        [(if ,[Pred -> uvp] ,[uvc] ,[uva]) 
         `(if ,uvp ,uvc ,uva)]
        [(begin ,[uve*] ... ,[uvv]) 
         `(begin ,uve* ... ,uvv)]
        [(mset! ,[Value -> uv1] 
                ,[Value -> uv2] 
                ,[Value -> uv3])
         `(mset! ,uv1 ,uv2 ,uv3)]
        [(,[Value -> uv1] ,[Value -> uv*] ...) 
         `(,uv1 ,uv* ...)]
        [,ef (error who "invalid Effect ~s" ef)])))

  (define Pred
    (lambda (pr)
      (match pr
        [(true) '(true)]
        [(false) '(false)]
        [(let ([,bind* ,[Value -> ret*]] ...) ,[ef])
         (make-begin `((set! ,bind* ,ret*) ... ,ef))]
        [(if ,[Pred -> uvp] ,[uvc] ,[uva]) 
         `(if ,uvp ,uvc ,uva)]
        [(begin ,[Effect -> uve*] ... ,[uvv]) 
         `(begin ,uve* ... ,uvv)]
        [(,relop ,[Value -> uv1] ,[Value -> uv2])
         `(,relop ,uv1 ,uv2)]
        [,pr (error who "invalid Pred ~s" pr)])))

  (define Tail
    (lambda (tail)
      (match tail
        [(let ([,bind* ,[Value -> ret*]] ...) ,[v])
         (make-begin `((set! ,bind* ,ret*) ... ,v))]
        [(if ,[Pred -> uvp] ,[uvc] ,[uva]) 
         `(if ,uvp ,uvc ,uva)]
        [(begin ,[Effect -> uve*] ... ,[uvv]) 
         `(begin ,uve* ... ,uvv)]
        [(sra ,[Value -> uvv] ,y) `(sra ,uvv ,y)]
        [(,binop ,[Value -> uv1] ,[Value -> uv2])
         `(,binop ,uv1 ,uv2)]
        [(alloc ,[Value -> uv]) `(alloc ,uv)]
        [(,[Value -> uv1] ,[Value -> uv*] ...) 
         `(,uv1  ,uv* ...)]
        [,e e])))
  (define remove-let
    (lambda (x)
      (match x
        [(letrec ([,label* (lambda (,uv* ...) ,[Tail -> bd*])] ...) 
           ,[Tail -> bd])
         `(letrec ([,label* (lambda (,uv* ...) ,bd*)] ...)
            ,bd)]
        [,x (error who "invalid Program ~s" x)])))  

  
  
  
  
  
  
  
  
  
  
  
  
  );; end lib
