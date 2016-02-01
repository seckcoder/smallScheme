(library 
  (Compiler remove-complex-opera*)
  (export remove-complex-opera*)
  (import 
        (Compiler my-helpers)
    (Framework match)
    (Framework helpers)
    (chezscheme))

;; INPUT 
;;
;;Program
;;  |(letrec ([label (lambda (uvar*) Body)]*) Body)
;;Body
;;  |(locals (uvar*) Tail)
;;tail
;;  |Triv
;;  |(binop Value Value)
;;  |(Value Value*)
;;  |(if Pred Tail Tail)
;;  |(begin Effect* Tail)
;;Pred
;;  |(true)
;;  |(false)
;;  |(relop Value Value)
;;  |(if Pred Pred Pred)
;;  |(begin Effect* Pred)
;;Effect
;;  |(nop)
;;  |(set! uvar Value)
;;  |(if Pred Effect Effect)
;;  |(begin Effect* Effect)
;;Value 
;;  |Triv
;;  |(binop Value Value)
;;  |(if Pred Value Value)
;;  |(begin Effect* Value)
;;Triv 
;;  |uvar 
;;  | int 
;;  | label
;;
;;  OUTPUT
;;
;;Program
;;  |(letrec ([label (lambda (uvar*) Body)]*) Body)
;;Body
;;  |(locals (uvar*) Tail)
;;Tail
;;  |Triv
;;  |(binop Triv Triv)
;;  |(Triv Triv*)
;;  |(if Pred Tail Tail)
;;  |(begin Effect* Tail)
;;Pred(true)
;;  |(false)
;;  |(relop Triv Triv)
;;  |(if Pred Pred Pred)
;;  |(begin Effect* Pred)
;;Effect(nop)
;;  |(set! uvar Value)
;;  |(if Pred Effect Effect)
;;  |(begin Effect* Effect)
;;Value
;;  |Triv
;;  |(binop Triv Triv)
;;  |(if Pred Value Value)
;;  |(begin Effect* Value)
;;Triv
;;  |uvar 
;;  |int 
;;  |label



  (define who 'remove-complex-opera*)

  (define remove-complex-opera*
    (lambda (expr)
      (match expr
        [(letrec ([,labels (lambda (,e1 ...) ,[Body -> body])] ...) ,[Body -> body*])
         `(letrec ([,labels (lambda (,e1 ...) ,body)] ...) ,body*)]
        [,e (errorf who "Error with ~s" e)])))

  
  (define Value
    (lambda (value)
      (match value
        [(alloc ,[Value->triv -> lab vlab val])
         (values vlab (make-begin `(,val ... (alloc ,lab))))]
        [(mref ,(Value->triv -> lab1 vlab1 val1) ,(Value->triv -> lab2 vlab2 val2))
         (values `(,vlab1 ... ,vlab2 ...) (make-begin `(,val1 ... ,val2 ... (mref ,lab1 ,lab2))))]
        [(begin ,[Effect -> t** ef*] ... ,[t* val])
         (values `(,t** ... ... ,t* ...) (make-begin `(,ef* ... ,val)))]
        [(if ,[Pred -> pv* pred] ,[tt* tc*] ,[ft* fc*])
         (values `(,pv* ... ,tt* ... ,ft* ...)
                 `(if ,pred ,tc* ,fc*))]
        [(,op ,[Value->triv -> t1 t1* c1] ,[Value->triv -> t2 t2* c2])
         (guard (binop? op))
         (values `(,t1* ... ,t2* ...)
                 (make-begin `(,c1 ... ,c2 ... (,op ,t1 ,t2))))]
        [,triv (guard (triv? triv)) (values '() triv)]
        [(,[Value->triv -> t1 t1* c1] ,[Value->triv -> t* t** c*] ...)
         (values `(,t1* ... ,t** ... ...)
                 (make-begin `(,c1 ... ,c* ... ... (,t1 ,t* ...))))]
        [,e (errorf who "Value ~s" e )])))
 
  (define Effect
    (lambda (expr)
      (match expr 
        [(nop) (values '() '(nop))]
        [(mset! ,(Value->triv -> lab1 vlab1 val1) 
                ,(Value->triv -> lab2 vlab2 val2)
                ,(Value->triv -> lab3 vlab3 val3))
         (values `(,vlab1 ... ,vlab2 ... ,vlab3 ...)
                 (make-begin `(,val1 ... ,val2 ... ,val3 ... (mset! ,lab1 ,lab2 ,lab3))))]
        [(if ,[Pred -> pv* pred] ,[t* t] ,[f* f])
         (values `(,pv* ... ,t* ... ,f* ...) `(if ,pred ,t ,f))]
        [(begin ,[ef* eff] ... ,[e* e])
         (values `(,ef* ... ... ,e* ...)
                 (make-begin `(,eff ... ,e)))]
        [(set! ,uvar ,[Value -> v* v ])
         (values v* `(set! ,uvar ,v))]
        [(,[Value->triv -> t1 t1* c1] ,[Value->triv -> t2 t2* c2] ...)
         (values `(,t1* ... ,t2* ... ...)
                 (make-begin `(,c1 ... ,c2 ... ... (,t1 ,t2 ...))))]
        [,e (errorf who "Effect ~s " e)])))
  
  (define Pred 
    (lambda (expr)
      (match expr
        [(true) (values '() '(true))]
        [(false) (values '() '(false))]
        [(if ,[p* pred] ,[t* t] ,[f* f])
         (values `(,p* ... ,t* ... ,f* ...) `(if ,pred ,t ,f))]
        [(begin ,[Effect -> eff* eff] ... ,[p* pred])
         (values `(,eff* ... ... ,p* ...) 
                 (make-begin `(,eff ... ,pred)))]
        [(,op ,[Value->triv -> t1 t1* c1] ,[Value->triv ->  t2 t2* c2])
         (values `(,t1* ... ,t2* ...)
                 (make-begin `(,c1 ... ,c2 ... (,op ,t1 ,t2))))]
        [,e (errorf who "Pred ~s" e)])))
  
  (define Tail
    (lambda (expr)
      (match expr
        [(alloc ,[Value->triv -> lab vlab val])
         (values vlab (make-begin `(,val ... (alloc ,lab))))]
        [(mref ,(Value->triv -> lab1 vlab1 val1) ,(Value->triv -> lab2 vlab2 val2))
         (values `(,vlab1 ... ,vlab2 ...) (make-begin `(,val1 ... ,val2 ... (mref ,lab1 ,lab2))))]
        [(if ,[Pred -> p* pred] ,[t* t] ,[f* f])
         (values `(,p* ... ,t* ... ,f* ...) `(if ,pred ,t ,f))] 
        [(begin ,[Effect -> eff* eff] ... ,[t* tail])
         (values `(,eff* ... ... ,t* ...) 
                 (make-begin `(,eff ... ,tail)))]
        [(,op ,[Value->triv -> t1 t1* c1] ,[Value->triv ->  t2 t2* c2])
         (guard (binop? op))
         (values `(,t1* ... ,t2* ...)
                 (make-begin `(,c1 ... ,c2 ... (,op ,t1 ,t2))))]
        [(,[Value->triv -> t1 t1* c1] ,[Value->triv -> t2 t2* c2] ...)
         (values `(,t1* ... ,t2* ... ...)
                 (make-begin `(,c1 ... ,c2 ... ... (,t1 ,t2 ...))))]
        [,triv (guard (triv? triv)) (values '() triv)]
        [,e (errorf who "Tail ~s" e)])))
  
  (define Body
    (lambda (expr)
      (match expr
        [(locals ,uvars* ,[Tail -> tail* tail])
         `(locals (,tail* ... ,uvars* ...) ,tail )]
        [,e (errorf who "Body  ~s" e)])))
 
  ;; (new-label, list of all labels, new code to be put in)
  (define Value->triv 
    (lambda (val)
      (if (triv? val)
        (values val '() '())
        (let-values ([(t) (unique-name 't)]
                     [(t* val) (Value val)])
          (values t `(,t . ,t*) `((set! ,t ,val)))))))
  
  (define binop?
    (lambda (x)
      (memq x '(+ - * logand logor sra))))
  
  (define triv?
    (lambda (x)
    (ort x label? uvar? int64?))) 
  );; end lib
