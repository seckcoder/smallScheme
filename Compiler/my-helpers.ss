(library
  (Compiler my-helpers)
  (export 
   curry 
   begin-values
   andt
   ort
   define-with-implicits
   ==>
   dbg
   my-max
   map-values
   flatten*
   union*
   immediatefixnum?
   value-prim?
   effect-prim?
   pred-prim?
   prim?
   immediate?
   make-set
   member?
   member*
   get
   get-imm
   mmake-begin
   )
  (import
    (Framework helpers)
    (Framework match)
    (chezscheme))

  (define mmake-begin
  (lambda (ls)
    (let ((ls (remove '(nop) ls)))
      (if (null? ls)
        '(nop)
        (make-begin ls)))))

  (define get-imm
    (lambda (x)
      (match x
        [(quote ,i) i]
        [,_ _])))

  (define get
    (lambda (env y)
      (match env
        [() y]
        [((,x ,a) . ,env) (if (eq? x y) a (get env y))]
        [,x x])))

  (define member? 
    (lambda (item ls)
      (cond
        [(null? ls) #f]
        [(pair? (car ls)) (or (member? item (car ls))
                              (member? item (cdr ls)))]
        [(eq? (car ls) item) #t]
        [else (member? item (cdr ls))])))

  (define member*
    (lambda (item ls)
      (cond
        [(null? ls) #f]
        [(pair? (car ls)) (or (member* item (car ls))
                              (member* item (cdr ls)))]
        [(eq? (car ls) item) ls]
        [else (member* item (cdr ls))])))


  (define make-set
    (lambda (ls)
      (cond
        [(null? ls) '()]
        [else (set-cons (car ls) (make-set (cdr ls)))])))

  (define immediate?
    (lambda (x)
      (or (fixnum? x) (int64? x) (and (memq x '(() #t #f)) #t))))

  (define prim?
    (lambda (prim)
      (and (or (pred-prim? prim)
               (effect-prim? prim)
               (value-prim? prim)) #t)))



  (define flatten*
    (lambda (ls)
      (cond
        [(null? ls) '()]
        [(pair? (car ls)) `(,@(flatten* (car ls)) ,@(flatten* (cdr ls)))]
        [(null? (car ls)) (flatten* (cdr ls))]
        [else (cons (car ls) (flatten* (cdr ls)))])))

  (define union*
    (lambda (ls . args)
      (let loop ([ls ls] [ans '()] [args* args])
        (cond
          [(null? args*) (union ls ans)]
          [else
            (let ([ls^ (if (list? ls) (flatten* ls) `(,ls))])
              (loop (car args*) (union ls^ ans) (cdr args*)))]))))

         (define immediatefixnum?
           (lambda (x)
             (or (fixnum? x) (and (memq x '(() #t #f)) #t))))

         (define value-prim?
           (lambda (x)
             (and  (memq x '(+ - * car cdr cons make-procedure procedure-code procedure-ref make-vector vector-length vector-ref void)) #t)))

         (define effect-prim?
           (lambda (x)
             (and  (memq x '(procedure-set! set-car! set-cdr! vector-set!)) #t)))

         (define pred-prim?
           (lambda (x)
             (and  (memq x '(procedure? < <= = >= > boolean? eq? fixnum? null? pair? vector?)) #t)))

  ;; Same as map. Except now f can be a multiple value producing function.
  ;; returns values
  (define map-values
    (lambda (f ls)
      (define return-values
        (lambda (vals vals*)
          (if (null? vals*)
            (apply values (map list vals))
            (apply values (map cons vals vals*)))))
      (let mapper ([ls ls])
        (if (null? ls)
          (values)
          (let-values ([vals (f (car ls))] [vals* (mapper (cdr ls))])
            (return-values vals vals*))))))
  
  
  ;; Given a list of numbers, this return the maximum number in the list.
  (define my-max
    (lambda (ls)
      (cond
        [(null? ls) 0]
        [else (apply max ls)])))

  ;; We give it a level and name to error out with, 
  ;; a msg and all the args that we want
  ;; The environment variable DEBUG must be set to whatever 
  ;; level of debugging you want.
  (define dbg
    (lambda (dbg-lvl name msg . args)
       (cond
         [(getenv "DEBUG") 
          => (lambda (x) (if (<= dbg-lvl (string->number x)) 
          (errorf name (format (string-append msg " ~{~a ~}") args))))]
         [else (void)])))

  ;; Andre inspired me to write this handy little macro.
  ;; ==> or "implies" (not really...)
  ;; so (==> (x : zero? integer?) (y : exact? int64?))
  ;; => (and (andt x zero? integer?) (ort y exact? int64?))
  (define-syntax ==> 
    (syntax-rules (:)
      ((_ (var : p* ...) (var2 : e* ...))
       (and (ort var p* ...) (ort var2 e* ...)))))


  ;; (define-with-implicits <nm>
  ;;   (imp*)
  ;;   (lambda (arg*)
  ;;      b+))
  ;; Where:
  ;;  - <imp*> are valid formal parameter(s) to scheme procedures
  ;;  - <arg*> are valid formal parameter(s) to scheme procedures
  ;;  - <b+> are valid scheme bod(y/ies)
  ;; Implicitly defines the values (imp* ...)
  ;; so that when we are inside the function we only
  ;; have to call the function with (f ...) yet we also
  ;; have acces to (imp* ...) within the body.
  ;; NOTE: outside functions MUST call the function with
  ;; (nm f ... imp* ...)
  ;; Example:
  ;; (define-with-implicits foo
  ;;   (e r)
  ;;    (lambda (x)
  ;;     (if (zero? x)
  ;;      (list e r x)
  ;;      (foo (sub1 x)))))
  ;;The example call of this function would be:
  ;; (foo 4 5 6) => (5 6 0)
  (define-syntax define-with-implicits
    (syntax-rules (lambda)
      ((_ nm (imp* ...) (lambda (f ...) b b* ...))
       (define nm
         (lambda (f ... imp* ...)
           (letrec ((nm (lambda (f ...)
                          b b* ...)))
             (nm f ...)))))))

  ;; We give it a variable and then a bunch of functions.
  ;; It runs each function on x and then and's everything together
  ;; Example call:
  ;; (andt 5
  ;;   zero?
  ;;   number?) => #f
  (define andt
    (lambda (x . rest)
      (andmap (lambda (f) (if (f x) #t #f)) rest)))

  ;; We give it a variable and then a bunch of functions.
  ;; It runs each function on x and then or's everything together
  ;; Example call:
  ;; (ort 5
  ;;   zero?
  ;;   number?) => #t
  (define ort
    (lambda (x . rest)
      (ormap (lambda (f) (if (f x) #t #f)) rest)))

  ;; (curry (e+) b b* ...)
  ;; e ::= t | (t*)
  ;; where t is a valid formal parameter in scheme
  ;; and b b* ... are valid scheme bodies.
  ;;
  ;; Transforms an expression as follows:
  ;; (curry (e e* ...) b b* ...)
  ;; = (lambda (e)
  ;;    (curry (e* ...)
  ;;      b b* ...))
  ;; So (curry (d (f e)) (+ d f e))
  ;; => (lambda (d)
  ;;     (lambda (f e)
  ;;       (+ d f e))) 
  (define-syntax curry
    (syntax-rules (:)
      ((_ ((e ...) . rest) b b* ...)
       (lambda (e ...)
         (curry rest
                b b* ...)))
      ((_ (v1) b b* ...)
       (lambda (v1) b b* ...))
      ((_(v1 v2 ...) b b* ...)
       (lambda (v1)
         (curry (v2 ...) b b* ...)))))

  ;; Returns values, where each value is evaluted after the
  ;; one to the left of it and before the one to the right is evaluated.
  ;; E.g:
  ;; (set! env 0)
  ;; (begin-values
  ;;  (begin (set! env (add1 env)) env)
  ;;  (begin (set! env (add1 env)) env)
  ;;  (begin (set! env (add1 env)) env))
  ;; => 1 2 3
  ;; Whereas regular values will return:
  ;; => 3 2 1 
  (define-syntax (begin-values x)
    (syntax-case x ()
      ((_ e) #'(values e))
      ((_ e1 e2)
       #'(let* ((e1^ e1)
                (e2^ e2))
           (values e1^ e2^)))
      ((_ e e* ...)
       (with-syntax ([(n n* ...) (generate-temporaries #'(e e* ...))])
          #'(let* ((n e) 
                   (n* e*) ...)
              (values n n* ...))))))
  ) ;; end lib

