(library
  (Compiler finalize-locations)
  (export finalize-locations)
  (import (chezscheme)
    (Compiler my-helpers)
    (Framework helpers)
    (Framework match))
  ;; All we do in this pass is go through the grammar and replace any occurence of a
  ;; uvar with the corresponding variable (reg | fvar) located in the locate form of the
  ;; corresponding body.
  (define who 'finalize-locations)

  (define get-location 
    (curry (env var)
      (if (uvar? var) (cdr (assq var env)) var)))

  (define Pred
    (curry (env prd) 
      (match prd
        [(false) '(false)]
        [(true) '(true)]
        [(begin ,[(Effect env) -> eff*] ... ,[p])
         `(begin ,eff* ... ,p)]
        [(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
        [(,relop ,[(get-location env) -> trv1] ,[(get-location env) -> trv2]) 
         `(,relop ,trv1 ,trv2)]
        [,else (errorf who "Error in Pred with ~s" else)])))

  (define Tail 
    (curry (env tl)
      (match tl
        [(begin ,[(Effect env) -> eff*] ... ,[tail])
         `(begin ,eff* ... ,tail)]
        [(if ,[(Pred env) -> t] ,[c] ,[a]) `(if ,t ,c ,a)]
        [(,[(get-location env) -> triv]) `(,triv)]
        [,else (errorf who "Error in Tail with ~s" else)])))

  (define Effect
    (curry (env efct)
      (match efct
        [(nop) '(nop)]
        [(begin ,[eff*] ... ,[eff])
         `(begin ,eff* ... ,eff)]
        [(if ,[(Pred env) -> t] ,[c] ,[a]) 
         `(if ,t ,c ,a)]
        [(set! ,[(get-location env) -> var] 
           (,bop ,[(get-location env) -> trv1]
                 ,[(get-location env) -> trv2]))
         `(set! ,var (,bop ,trv1 ,trv2))]
        [(set! ,[(get-location env) -> v] ,[(get-location env) -> trv])
         (if (eqv? v trv) '(nop) `(set! ,v ,trv))]
        [(mset! ,[(get-location env) -> t1] 
               ,[(get-location env) -> t2]
               ,[(get-location env) -> t3])
         `(mset! ,t1 ,t2 ,t3)]
        [(return-point ,rpl ,[(Tail env) -> tl]) `(return-point ,rpl ,tl)]
        [,else (errorf who "Error in Effect with ~s" else)])))

  (define Body
    (lambda (bod)
      (match bod
        [(locate ([,uv* ,loc*] ...) ,tail)
         ((Tail (map cons uv* loc*)) tail)]
        [,else (errorf who "Error in Body with ~s" else)])))

  (define finalize-locations 
    (lambda (prog)
      (match prog
        [(letrec ([,lbl* (lambda () ,[Body -> bod*])] ...) ,[Body -> bod])
         `(letrec ([,lbl* (lambda () ,bod*)] ...) ,bod)]
        [,else (errorf who "Error in program with ~s" else)])))







  ) ;; end lib
