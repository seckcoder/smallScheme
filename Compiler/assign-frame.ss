 (library
  (Compiler assign-frame)
  (export assign-frame get-homes get-used get-fvar)
  (import
   (chezscheme)
   (Framework match)
   (Framework helpers)
   (Compiler my-helpers))
 

(define who 'assign-frame)

(define assign-frame
  (lambda (prog)
    (match prog
      [(letrec ([,lab* (lambda () ,[Body -> b*])] ...) ,[Body -> b])
       `(letrec ([,lab* (lambda () ,b*)] ...) ,b)]
      [,else (errorf who "Error with ~s" else) ])))

(define Body
  (lambda (bd)
    (match bd
      [(locals (,uv* ...)
           (ulocals (,uloc* ...)
             (spills (,spill* ...)
               (locate (,assed ...)
                 (frame-conflict ,cf ,tl)))))
         (let ([homes (get-homes spill* cf assed)])
           `(locals (,uv* ...)
              (ulocals (,uloc* ...)
                (locate (,homes ...)
                  (frame-conflict ,cf ,tl)))))]
        [(locate (,assed ...) ,body) `(locate (,assed ...) ,body)])))

;; Basically the same idea as assign-registers except we can be more cavalier 
;; since we have "infinitely" many frame-variables
(define get-homes
  (lambda (vars cf assed)
    (if (null? vars)
      assed ;; Ok return them back out then 
      (let* ([var (car vars)] 
             ;; We dont need to pick a low degree node, so pick the first one 
             [conflicts (cdr (assq var cf))]
             ;; Get Vars conflict-graph
             [used (get-used conflicts assed)]
             ;; get the ones that he conflicts with
             [ass (get-fvar used)]) ;; Now assign him
        (get-homes (cdr vars) cf (cons `(,var ,ass) assed)))))) ;; and tack him in

;; Same as in assign-registers, except we see if its a Frame-var
(define-with-implicits get-used (assed)
  (lambda (cf)
    (cond
      [(null? cf) '()]
      [(frame-var? (car cf))
       (set-cons (car cf) (get-used (cdr cf)))]
      [(assq (car cf) assed) 
       => (lambda (x) (set-cons (cadr x) (get-used (cdr cf))))]
      [else (get-used (cdr cf))])))

;; Since we have as many frame-vars as we want we just take the
;; max + 1 of the assigned frame-vars
(define get-fvar
  (lambda (assed-ones)
    (let ((nums (map frame-var->index assed-ones)))
      (index->frame-var (add1 (my-max nums))))))

   
   );; end lib 
