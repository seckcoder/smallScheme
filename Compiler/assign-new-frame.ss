(library 
  (Compiler assign-new-frame)
  (export assign-new-frame)
  (import
   (Compiler my-helpers)
   (Framework match)
   (Framework helpers)
   (chezscheme))
 
  (define who 'assign-new-frame)


  (define assign-new-frame 
    (lambda (prog)
      (match prog
        [(letrec ([,label (lambda () ,[Body -> body*])] ...) ,[Body -> body])
         `(letrec ([,label (lambda () ,body*)] ...) ,body)])))

  (define Body
    (lambda (bod)
      (match bod
        [(locals ,uvar*
           (new-frames ,frames*
             (locate ,home*
               (frame-conflict ,fc
                 (call-live ,live ,t)))))
         (let* ([max-size (get-max home*)]
                [tail (Tail t max-size)]
                [homes (make-homes frames* max-size)])
           `(locals ,(difference uvar* `(,frames* ... ...))
              (ulocals ()
                (locate (,homes ... ,home* ...)
                  (frame-conflict ,fc ,tail)))))]
        [,e (error who "Body ~s" e)])))
  
  (define get-max
    (lambda (fvars)
      (cond
        [(null? fvars) 1]
        [else (add1 (my-max (map frame-var->index (map cadr fvars))))])))

  (define Tail 
    (lambda (tail max-index)
      (match tail
        [(return-point ,rp-label ,tail)
         (make-begin 
           (let ([incr (ash max-index word-shift)])
             ;; move up for the calle
             `((set! ,frame-pointer-register (+ ,frame-pointer-register ,incr))
               (return-point ,rp-label ,tail)
               ;;ok, now move it back down
               (set! ,frame-pointer-register (- ,frame-pointer-register ,incr)))))]
        [(,[a] . ,[d]) `(,a . ,d)]
        [,_ _]))) ;; OTW just spit it back out


  ;; creates the offset for the frames
  (define make-offsets
    (lambda (frames incr)
      (cond
        [(null? frames) '()]
        [else 
          (cons `(,(car frames) ,(index->frame-var incr))
                (make-offsets (cdr frames) (add1 incr)))])))

  (define-with-implicits make-homes (max-index)
    (lambda (frames)
      (cond
        [(null? frames) '()]
        [else `(,@(make-offsets (car frames) max-index) . ,(make-homes (cdr frames))) ])))



  ) ;; end lib
