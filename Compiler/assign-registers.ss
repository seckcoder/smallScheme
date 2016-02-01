(library
  (Compiler assign-registers)
  (export assign-registers)
  (import
   (chezscheme)
   (Framework match)
   (Framework helpers)
   (Compiler Optimizations move-biasing)
   (Compiler my-helpers))

  ;; The definition and corresponding documentation of define-with-implicits can be
  ;; found in Compiler/my-helpers.ss
  (define who 'assign-registers)
  (define k (length registers))

  ;; Given a conflict graph, we return the used registers
  (define-with-implicits get-used (homes)
    (lambda (cf)
      (cond
        [(null? cf) cf] ;; then just return it
        [(register? (car cf)) ;; then put it in
         ;; make sure we dont have duplicate registers, so 
         ;; use set-cons
         (set-cons (car cf) (get-used (cdr cf)))]
        [(assq (car cf) homes) 
         ;;Weve assigned this guy to a reg,
         ;;so we'll put his reg in
         => (lambda (x)
              (set-cons (cadr x) (get-used (cdr cf))))]
        [else (get-used (cdr cf))])))

  ;; Given a var a conflict graph and a list of"homes"
  ;; we return a register that we can safely use.
  (define get-reg
    (lambda (var cf homes)
      (let* ([used (get-used cf homes)] ;; Get all the used registers
             [can-use (difference registers used)]) ;; These are the ones we can use
        (and (not (null? can-use)) (car can-use))))) ;; For some reason the when was not working here 
  ;; We were return void... This seemed to work....

  ;; Given two conflict graphs, and a var
  ;; we do: for each v in cf we (mutatively) remove that var from 
  ;; from the corresponding c-graph (for v wich is a subset of tcf)
  ;; in the first conflict graph passed in
  (define remove-conflicts
    (lambda (tcf var cf)
      (for-each
        (lambda (v) 
          (when (uvar? v) 
            (set-cdr! (assq v tcf) (remove var (cdr (assq v tcf))))))
        cf)))

  ;; Given a conflict graph and a list of uvars,
  ;; we return a list of homes (assignments) for those uvars.
  (define-with-implicits get-homes (tcf)
    (lambda (uvars)
      ;;A little helper. Self explanatory, We use an inner define
      ;;so we have acces to tcf
      (define low-degree?
        (lambda (var)
          (< (length (cdr (assq var tcf))) k)))
      
      (if (null? uvars) 
          '() ;; Were all done, so return something we can start consing into
          (let* ([var (cond
                        [(find low-degree? uvars)] 
                        ;; If we can find a low degree node, take it.
                        [else (car uvars)])] ;; OTW pick whatever is first...
                 [cf (cdr (assq var tcf))] ;; get all our conflicts for this guy
                 [uvars (remove var uvars)]) ;; we now have one less guy to find a home for
            (remove-conflicts tcf var cf) ;; So remove this guys conflicts from tcf
            (let* ([homes (get-homes uvars)] 
                   ;; recur now that we've removed the var from uvars and the conflict graph
                   [reg (get-reg var cf homes)]) 
              ;; ok weve picked the regs for everyone else, now its our turn
              (if reg ;; if we have one
                  (cons `[,var ,reg] homes) ;; then cons him in (nice brackets ;) ...)
                  homes)))))) 

  (define Body
    (lambda (exp)
      (match exp
        [(locate ,uvars ,tail) `(locate ,uvars ,tail)]
        [(locals ,local*
           (ulocals ,ulocal*
             (locate ,frame-home*
               (frame-conflict ,fv-ct
                 (register-conflict ,ct ,tail)))))
         ;;set the length or registers from the register list in the helprs file
         (let* ([uvar* (append local* ulocal*)])
           (let ([home* (get-homes uvar* ct)])
             (let ([spill* (difference uvar* (map car home*))])
               (cond
                 [(null? spill*) `(locate (,@frame-home* ,@home*) ,tail)]
                 [(null? (intersection ulocal* spill*))
                  (let ([local* (difference local* spill*)])
                    `(locals ,local*
                       (ulocals ,ulocal*
                         (spills ,spill*
                           (locate ,frame-home*
                             (frame-conflict ,fv-ct ,tail))))))]
                 [else (errorf who "Body of assign-registers ~s" exp)]))))])))


  (define assign-registers
    (lambda (prog)
      (match prog
        [(letrec ([,lbl* (lambda () ,[Body -> bod*])] ...) ,[Body -> bod])
         `(letrec ([,lbl* (lambda () ,bod*)] ...) ,bod)]
        [,else (errorf who "Error in assign-registers with ~s" else)]))) 
  );; end lib
