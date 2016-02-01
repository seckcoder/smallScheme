(library 
  (Compiler uncover-common)
  (export  uncover)
  (import 
    (Framework helpers)
    (Framework match)
    (Compiler my-helpers)
    (chezscheme))

  ;; We fold-right on the begins in order to keep the order of starting at the bottom and working our way up

  ;; We use inner defines so that we dont have a global conflict graph (or have to
  ;; thread it through everything). This then 
  ;; forces us to have the majority of our program inside the define (including Tail)
  ;; which forces us to pass the tail through.
  
  ;; We first create a conflict-graph c-graph
  (define uncover
    (lambda (test uvars t*)
      (let ((c-graph (map (lambda (x) `(,x)) uvars))
            (call-live '()))

        ;; a little function that does all the additions to our live set
        (define add-l   
          (lambda (test live . rest)
            (fold-left 
              (lambda (x y)
                (cond
                  [(pair? y) (union y x)]
                  [(or (test y) (uvar? y)) (set-cons y x)]
                  [else x]))
              live 
              rest)))

        ;; If its a uvar or register we add it in, otherwise, we do nothing
        (define make-conflict
          (lambda (test lookup add)
            (when (and (uvar? lookup) (or (uvar? add) (test add)))
              (let ([new (assq lookup c-graph)])
                (when new
                  (set-cdr! new (set-cons add (cdr new)))))))) ;; updating the assoc list that we previously had

        ;; updates our graph with a conflict and the current live set we have
        (define update-conf-graph 
          (lambda (test var live) 
            (map (lambda (x) 
                   (make-conflict test var x) 
                   (make-conflict test x var)) 
                 live)))

        (define Tail
          (lambda (test t live) 
            (match t
              [(if ,pred ,[c] ,[a]) (Pred test pred c a)]
              [(begin ,eff* ... ,[tail]) (fold-right (Effect test) tail eff*)]
              [(,triv ,loc* ...)  (add-l test live triv loc*)]
              [,t live])))
        ;; As alluded to on pg 7 of the the assignment decription, we pass around
        ;; a true and false live set
        (define Pred
          (lambda (test p t-live f-live)
            (match p
              [(true) t-live]
              [(false) f-live]
              [(begin ,eff* ... ,[pred]) (fold-right (Effect test) pred eff*)]
              [(if ,pred ,[p1] ,[p2]) (Pred test pred p1 p2)]
              [(,relop ,triv1 ,triv2) (add-l test (union t-live f-live) triv1 triv2)])))

        (define Effect
          (lambda (test)
          (lambda (e live)
            (match e
              [(nop) live]
              [(set! ,var (,op ,t1 ,t2))         
               (let ([l-live (remove var live)]) 
                 ;; remove lhs from live set (since hes being assigned to) and is now
                 ;; dead
                 (update-conf-graph test var l-live) ;; update the conflict graph with this guy
                 ;; This guy now conflicts with anything that is currently live
                 (add-l test l-live t1 t2))] ;; t1 and t2 have now become live
              [(set! ,var ,triv)
               (let ([l-live (remove var live)]) 
               ;; hes being assigned to, so he is no longer live
                 (update-conf-graph test var l-live)
                 ;; He now conflicts with anything that is currently live
                 (add-l test l-live triv))];; and triv is now live            
              [(mset! ,t1 ,t2 ,t3)
               (add-l test live t1 t2 t3)]
              [(if ,pred ,[eff1] ,[eff2]) (Pred test pred eff1 eff2)] 
              [(begin ,eff* ... ,[ef]) (fold-right (Effect test) ef eff*)]
              [(return-point ,r-lab ,tl)
               (let ((live-set (Tail test tl live)))
                 (set! call-live (union call-live live))
                 (union (filter (lambda (x) (ort x uvar? frame-var? register?)) call-live) live-set))]))))
        (Tail test t* '())
        (values c-graph call-live)))
    ))
