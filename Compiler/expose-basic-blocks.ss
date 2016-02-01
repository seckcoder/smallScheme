(library
  (Compiler expose-basic-blocks)
  (export expose-basic-blocks)
  (import (chezscheme)
          (Compiler my-helpers)
          (Framework helpers)
          (Framework match))
  
  
  (define who 'expose-basic-blocks)
  ;; The rule here is that all blocks will be returned as the RHS of the values ALWAYS
  ;;
  (define Tail
    (lambda (tail)
      (match tail
        [(begin ,eff* ... ,[tl tb]);; get tail, and tail blocks
         ;; Return back effects and effect blocks
         (let-values ([(ef efb*) (Effect* eff* (list tl))])
           ;; Append the effect blocks and tail blocks together
           (values ef `(,efb* ... ,tb ...)))]
        [(if ,p ,[c cb*] ,[a ab*])
         ;; Create predicate labels and send them into pred so that they can be used 
         ;; when we see (true), (false) and if we see a relop (since these become
         ;; if statements that jump to these labels)
         (let ([clab (unique-label 'c)] [alab (unique-label 'a)])
           (let-values ([(p pb*) (Pred p clab alab)])
             ;; Append the returned blocks with the "hardcoded" blocks that
             ;; we create for the consecuent and alternative labels 
             (values p `(,pb* ...
                              [,clab (lambda () ,c)]
                              ,cb* ...
                              [,alab (lambda () ,a)]
                              ,ab* ...))))]
        [(,triv) (values (list triv) '())]
        [,else (errorf "Error in Tail with ~s" else)])))
  
  (define Pred
    (lambda (pred tlab flab)
      (match pred
        ;; Dont need to return any blocks, since we just want to 
        ;; jump to flab 
        [(false) (values (list flab) '())]
        ;; Dont need to return any blocks, since we just want to 
        ;; jump to tlab 
        [(true) (values (list tlab) '())];;Same
        [(begin ,eff* ... ,[p pb*])
         ;; Return back effects and effect blocks
         (let-values ([(ef* efb*) (Effect* eff* (list p))])
           ;; Append the effect blocks and pred blocks together
           (values ef* `(,efb* ... ,pb* ...)))]
        [(if ,p ,[c cb*] ,[a ab*])
         ;; Create predicate labels and send them into pred so that they can be used 
         ;; when we see (true), (false) and if we see a relop (since these become
         ;; if statements that jump to these labels)
         (let ([alab (unique-label 'a)] [clab (unique-label 'c)])
           (let-values ([(prd pb*) (Pred p clab alab)])
             ;; Append the returned blocks with the "hardcoded" blocks that
             ;; we create for the consecuent and alternative labels 
             (values prd `(,pb* ...
                                [,clab (lambda () ,c)]
                                ,cb* ...
                                [,alab (lambda () ,a)]
                                ,ab* ...))))]
        [(,rop ,trv1 ,trv2)
         (values `(if (,rop ,trv1 ,trv2) (,tlab) (,flab)) '())]
        [,else (errorf who "Error in Pred with ~s" else)])))
  
  (define Effect
    (lambda (todo-effect* current-effect done-effects)
      (match current-effect
        [(nop) (Effect* todo-effect* done-effects)]
        [(begin ,eff* ...)
         (Effect* `(,todo-effect* ... ,eff* ...) done-effects)]
        [(if ,p ,c ,a) ;; Dear God.....
         (let ([clab (unique-label 'c)] 
               [alab (unique-label 'a)] 
               [jlab (unique-label 'join)])
           ;; Create labels not just for alternative and consequent, but we also 
           ;; need to "join" our effects, so wee need to create a label to jump to
           ;; for the block that we create with all the effects before the if.
           (let*-values ([(p pb*) (Pred p clab alab)]
                         ;; We dont have any effects in front of us so shoot through
                         ;; null. The done effects are now referrenced by the join 
                         ;; label since we have hit a point where we have to cut it
                         ;; off and start rebuilding done-effects.
                         [(c cb*) (Effect '() c `((,jlab)))]
                         [(a ab*) (Effect '() a `((,jlab)))]
                         ;; We want some way of connecting the "blocks to come"
                         ;; with the blocks that we have just created, so we want to 
                         ;; pass in p which will have jumps to some (all) of the
                         ;; blocks that we are defining here.
                         [(ef eb*) (Effect* todo-effect* `(,p))])
             ;; Append all our blocks together, as well as add in 
             ;; our join block. We use make-begin to make the code inside the
             ;; join block to be "nice" (i.e no nested paren nastiness...)
             (values ef `(,eb* ... ,pb* ...
                               [,clab (lambda () ,c)]
                               ,cb* ...
                               [,alab (lambda () ,a)]
                               ,ab* ...
                               [,jlab (lambda () ,(make-begin done-effects))]))))]
        ;; We dont need to do anything with a set!. So just put inside done-effects and 
        ;; keep on rollin
        [(set! ,l ,r) (Effect* todo-effect* `((set! ,l ,r) ,done-effects ...))]
        [(return-point ,rp-lab ,[Tail -> tl tlb*])
         (let*-values ([(rp rpb*) (Effect* todo-effect* `(,tl))])
           (values rp `(,rpb* ...
                        ,tlb* ...
                              [,rp-lab (lambda () ,(make-begin done-effects))])))]
        [,else (errorf who "Error in Effect with ~s" else)])))
  
  (define Effect*
    (lambda (todo-effect* done-effects)
      (match todo-effect*
        ;; Were have no more work to do, so just put a 
        ;; begin on done-effects and return it back out
        [() (values (make-begin done-effects) '())]
        ;; else pull an effect out and run it through Effect
        [(,eff* ... ,last) (Effect eff* last done-effects)]
        [,else (errorf who "Error in Effect* with ~s" else)])))
  
  (define expose-basic-blocks
    (lambda (prog)
      (match prog
        [(letrec ([,lbl* (lambda () ,[Tail -> tl* tb*])] ...) ,[Tail -> tl tb])
         `(letrec ([,lbl* (lambda () ,tl*)] ... ,tb* ... ... ,tb ...) ,tl)]
        [,else (errorf "Error at top level with ~s" else)])))
  ) ;; end lib
