(library 
  (Compiler flatten-program)
  (export flatten-program)
  (import 
    (chezscheme)
    (Compiler my-helpers)
    (Framework helpers)
    (Framework match))

  (define who 'flatten-program)
  ;; Here we just flatten out the rest of our program
  ;; The only lines that are interesting are the if and (triv)
  ;; lines
  ;; In the if line, we use the following relations:
  ;; if clab = next-lab ==> (if (not p) (jump alab))
  ;; if alab = next     ==> (if p (jump clab))
  ;;
  ;;In the (triv) line we return '() if it is equal to the next label
  ;;since in this case, we just want to fall through instead of jumping
  ;;ohterwise, we just return a jump to that label
  (define flatten-block
    (curry (env blok)
      (let* ([current-label (car blok)]
             [next-label (get-next current-label env)])
        (match blok
          [(begin ,[eff*] ... ,[tail]) `(,eff* ... ,tail ...)] 
          [(if ,p (,clab) (,alab))
           (cond
             [(eq? next-label clab) 
              `((if (not ,p) (jump ,alab)))]
             [(eq? next-label alab) 
              `((if ,p (jump ,clab)))]
             [else `((if ,p (jump ,clab)) (jump ,alab))])]
          [(set! ,l ,r) `(set! ,l ,r)]
          [(,lab (lambda () ,[expr1] ,[expr2] ...)) `(,lab ,expr1 ... ,expr2 ...)]
          [(,trv) (if (eq? next-label trv)
                    '() 
                    `((jump ,trv)))]
          [,else (errorf who "Error in flatten-block ~s" else)]))))
  
  ;; Entry point to flatten-program. We lop off the letrec.
  ;; "semi flatten" each tail and then rebind the labels with 
  ;; their respective tails. We then reorder and splice the various
  ;; tails together. 
  ;;   Notice that blok was matched last but now comes first!!
  ;; This is due to how X86-64 is set up, in that it is read from top down, 
  ;; and blok will be our "body" and (should) set everything up for the 
  ;; rest of the program.
  (define flatten-program
    (lambda (prog)
      (match prog
        [(letrec (,bd* ...) ,bd)
         (let ((lbl* (map car bd*)))
           `(code ,((flatten-block lbl*) bd) ... ,(map (flatten-block lbl*) bd*) ... ...))]
        [,else (errorf who "Error in Program with ~s" else)])))

  ;; Given a label L, and a list of labels, this either returns:
  ;; (a) The next label
  ;; (b) '(L)
  ;; (c) #f
  (define get-next
    (lambda (lbl env)
      (let ([env (memq lbl env)])
        (if (and env (not (null? (cdr env))))
          (cadr env)
          env))))
  ) ;; end lib
