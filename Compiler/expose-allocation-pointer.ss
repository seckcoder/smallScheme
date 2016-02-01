(library 
  (Compiler expose-allocation-pointer)
  (export expose-allocation-pointer)
  (import
    (Framework helpers)
    (Framework match)
    (Compiler my-helpers)
    (chezscheme))
  
;;Program 
;;    (letrec ([label (lambda (formals*) Body)]*) Body)
;;Body    
;;    (locals (uvar*) (new-frames (Frame*) Tail))
;;Frame   
;;    (uvar*)
;;Tail    
;;    (Triv Loc*)
;;  | (if Pred Tail Tail)
;;  | (begin Effect* Tail)
;;Pred    
;;    (true)
;;  | (false)
;;  | (relop Triv Triv)
;;  | (if Pred Pred Pred)
;;  | (begin Effect* Pred)
;;  | (label Triv*)
;;Effect
;;    (nop)
;;  | (set! Var Triv)
;;  | (set! Var (binop Triv Triv))
;;  | (set! Var (alloc offset))
;;  | (mset! Triv Triv Triv)
;;  | (return-point label Tail)
;;  | (if Pred Effect Effect)
;;  | (begin Effect* Effect)
;;  | (label Triv*)
;;Loc   reg  | fvar
;;Var   uvar | Loc
;;Triv  Var  | int | label:

;; Removes 'alloc' from the grammar.  Replaces it with direct
;; operations on the designated allocation-pointer-register.

;; Timothy Zakian P423 sp13 midterm exam. 

(define-who expose-allocation-pointer 
  
  (define Effect
    (lambda (x)
      (match x
        [(nop) '(nop)]
        [(set! ,uvar (alloc ,offset))
         (make-begin 
           `((set! ,uvar ,allocation-pointer-register)
             (set! ,allocation-pointer-register (+ ,allocation-pointer-register ,offset))))]
        [(set! ,uv1 (,binop ,uv2)) `(set! ,uv1 (,binop ,uv2))]
        [(set! ,uv ,triv) `(set! ,uv ,triv)]
        [(mset! ,triv1 ,triv2 ,triv3) `(mset! ,triv1 ,triv2 ,triv3)]
        [(return-point ,lab ,[Tail -> tail]) `(return-point ,lab ,tail)]
        [(if ,[Pred -> pred] ,[conseq] ,[altern])
         `(if ,pred ,conseq ,altern)]
        [(begin ,[effect*] ... ,[tail])
         `(begin ,effect* ... ,tail)]
        [(,lab ,triv* ...) `(,lab ,triv* ...)]
        [,e (errorf who "Effect ~s" e)])))

  (define Pred
    (lambda (x)
      (match x
        [(true) '(true)]
        [(false) '(false)]
        [(if ,[pred] ,[conseq] ,[altern])
         `(if ,pred ,conseq ,altern)]
        [(begin ,[Effect -> effect*] ... ,[tail])
         `(begin ,effect* ... ,tail)]
        [(,relop ,triv1 ,triv2) `(,relop ,triv1 ,triv2)]
        [(,lab ,triv* ...) `(,lab ,triv* ...)]
        [,e (errorf who "Pred ~s" e)])))

  (define Tail
    (lambda (x)
      (match x
        [(if ,[Pred -> pred] ,[conseq] ,[altern])
         `(if ,pred ,conseq ,altern)]
        [(begin ,[Effect -> effect*] ... ,[tail])
         `(begin ,effect* ... ,tail)]
        [(,triv ,loc* ...) `(,triv ,loc* ...)] 
        ;; We don't need to go into these since they are triv's and loc's
        ;; and therefore wont contain any alloc forms (according to the grammar)
        [,e (errorf who "Tail ~s" e)])))


  (define Body
    (lambda (x)
      (match x
        [(locals (,uv* ...) 
           (new-frames (,nfv* ...)
             ,[Tail -> tail]))
         `(locals (,uv* ...) 
           (new-frames (,nfv* ...)
             ,tail))]
        [,e (errorf who "Body ~s" e)])))

  (lambda (x) 
    (match x
      [(letrec ([,label* (lambda (,fml* ...) ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda (,fml* ...) ,body*)] ...) ,body)]
      [,e (errorf who "Error in expose with ~s" e)]))


)

  
  
  );end library

