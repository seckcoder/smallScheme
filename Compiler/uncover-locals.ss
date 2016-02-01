(library 
  (Compiler uncover-locals)
  (export  uncover-locals)
  (import 
    (Framework helpers)
    (Framework match)
    (Compiler my-helpers)
    (chezscheme))

  (define who 'uncover-locals)

(define Body
  (lambda (bod)
    (values bod (Tail bod))))

  (define Value
    (lambda (val)
      (match val
        [(let ([,new-uvar* ,[uv-ret*]] ...) ,[uv])
         `(,new-uvar* ... ,uv-ret* ... ... ,uv ...)]
        [(if ,[Pred -> uvp] ,[uvc] ,[uva]) 
         `(,uvp ... ,uvc ... ,uva ...)]
        [(begin ,[Effect -> uve*] ... ,[uvv]) 
         `(,uve* ... ... ,uvv ...)]
        [(sra ,[uvv] ,y) uvv] ;; might want to wrap
        [(,binop ,[uv1] ,[uv2])
         `(,uv1 ... ,uv2 ...)]
        [(alloc ,[uv]) uv]
        [(,[uv1] ,[uv*] ...) 
         `(,uv1 ... ,uv* ... ...)]
        [,e '()])))

  (define Effect
    (lambda (ef)
      (match ef
        [(nop) '()]
        [(let ([,new-uvar* ,[Value -> vuv]] ...) ,[euv])
         `(,new-uvar* ... ,vuv ... ... ,euv ...)]
        [(if ,[Pred -> uvp] ,[uvc] ,[uva]) 
         `(,uvp ... ,uvc ... ,uva ...)]
        [(begin ,[uve*] ... ,[uvf]) 
         `(,uve* ... ... ,uvf ...)]
        [(mset! ,[Value -> uv1] 
                ,[Value -> uv2] 
                ,[Value -> uv3])
         `(,uv1 ... ,uv2 ... ,uv3 ...)]
        [(,[Value -> uv1] ,[Value -> uv*] ...) 
         `(,uv1 ... ,uv* ... ...)]
        [,ef (error who "invalid Effect ~s" ef)])))

  (define Pred
    (lambda (pr)
      (match pr
        [(true) '()]
        [(false) '()]
        [(let ([,new-uvar* ,[Value -> vuv]] ...) ,[euv])
         `(,new-uvar* ... ,vuv ... ... ,euv ...)]
        [(if ,[Pred -> uvp] ,[uvc] ,[uva]) 
         `(,uvp ... ,uvc ... ,uva ...)]
        [(begin ,[Effect -> uve*] ... ,[uvf]) 
         `(,uve* ... ... ,uvf ...)]
        [(,relop ,[Value -> uv1] ,[Value -> uv2])
         `(,uv1 ... ,uv2 ...)]
        [,pr (error who "invalid Pred ~s" pr)])))

  (define Tail
    (lambda (tail)
      (match tail
        [(let ([,new-uvar* ,[Value -> vuv]] ...) ,[euv])
         `(,new-uvar* ... ,vuv ... ... ,euv ...)]
        [(if ,[Pred -> uvp] ,[uvc] ,[uva]) 
         `(,uvp ... ,uvc ... ,uva ...)]
        [(begin ,[uve*] ... ,[uvf]) 
         `(,uve* ... ... ,uvf ...)]
        [(sra ,[Value -> uv] ,y) uv]
        [(,binop ,[Value -> uv1] ,[Value -> uv2])
         `(,uv1 ... ,uv2 ...)]
        [(alloc ,[Value -> uv]) uv]
        [(,[Value -> uv1] ,[Value -> uv*] ...) 
         `(,uv1 ... ,uv* ... ...)]
        [,e '()])))
  (define uncover-locals
    (lambda (x)
      (match x
        [(letrec ([,label* (lambda (,uv* ...) ,[Body -> bd* new-loc*])] ...) 
           ,[Body -> bd new-locs])
         `(letrec ([,label* (lambda (,uv* ...) (locals ,new-loc* ,bd*)) ] ...)
            (locals ,new-locs ,bd))]
        [,x (error who "invalid Program ~s" x)])))  

  );; end lib
