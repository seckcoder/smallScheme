(library
  (Compiler convert-complex-datum)
  (export convert-complex-datum)
  (import
    (chezscheme)
    (Framework helpers)
    (Framework match)
    (Compiler my-helpers))

;;  INPUT: 
;;
;;Program
;;  |Expr
;;Expr
;;  |uvar
;;  |(quote datum)
;;  |(if Expr Expr Expr)
;;  |(begin Expr* Expr)
;;  |(lambda (uvar*) Expr)
;;  |(let ([uvar Expr]*) Expr)
;;  |(letrec ([uvar Expr]*) Expr)
;;  |(set! uvar Expr)
;;  |(prim Expr*)
;;  |(Expr Expr*)
;;
;;  OUTPUT:
;;
;;Program
;;  |Expr
;;Expr
;;  |uvar
;;  |(quote Immediate)
;;  |(if Expr Expr Expr)
;;  |(begin Expr* Expr)
;;  |Lambda
;;  |(let ([uvar Expr]*) Expr)
;;  |(letrec ([uvar Lambda]*) Expr)
;;  |(set! uvar Expr)
;;  |(prim Expr*)
;;  |(Expr Expr*)
;;Lambda
;;  |(lambda (uvar*) Expr)
;;immediate 
;;  | () 
;;  | #t 
;;  | #f

  (define who 'convert-complex-datum)

  (define handle-list
    (lambda (ls)
      (cond
        [(atom? ls) `(quote ,ls)]
        [else `(cons ,(if (immediate? (car ls)) `(quote ,(car ls)) (car ls)) ,(handle-list (cdr ls)))])))

  (define handle-vector
    (lambda (ptr v n)
      (cond
        [(null? v) (values '() n) ]
        [(immediate? v)  (values `((vector-set! ,ptr (quote ,n) (quote ,v))) n)]
        [(immediate? (car v))
         (let-values ([(a n^) (handle-vector ptr (car v) n)]
                      [(d _^) (handle-vector ptr  (cdr v) (add1 n))])
           (values `(,@a ,@d) _^))]
        [else
          (let-values ([(d _^) (handle-vector ptr  (cdr v) (add1 n))])
            (values `((vector-set! ,ptr (quote ,n)  ,(if (immediate? (car v)) `(quote ,(car v)) (car v))) ,@d) _^)) ])))

    (define handle-complex-datum
      (lambda (datum)
        (match datum
          [(,prim ,x) (guard (prim? prim)) `(,prim ,x)]
          [(quote ,[x]) x]
          [(,[d*] ...)
           (handle-list d*)]
          [(,[a] . ,[d]) (handle-list `(,a . ,d))]
          [#(,[d*] ...)
           (let*-values ([(tmp) (unique-name 'tmp)]
                         [(t) (unique-name 't)]
                         [(b l) (handle-vector tmp d* 0)])
                        `(let ([,tmp (make-vector (quote ,l))])
                           ,(make-begin `(,b ...  ,tmp))))]
          [,x (guard (ort x immediate? int64?)) x]
          [,e (errorf who "handle-complex-datum ~s" e)])))

    (define complex-datum?
      (lambda (datum)
        (match datum
          [(quote (,d* ...)) #t]
          [(quote #(,d* ...)) #t]
          [,_ #f])))

    (define convert-bindings
      (lambda (bindings)
        (match bindings
          [(,u (lambda (,u* ...) ,body))
           (let*-values ([(tmps e^) (convert-literals body)]
                         [(be e) (Expr e^)])
                        (values `(,tmps ... ,be ...) `(,u (lambda ,u* ,e))))]
          [(,u ,expr) (guard (ort u label? uvar?))
                      (let*-values ([(tmps e^) (convert-literals expr)]
                                    [(be e) (Expr e^)])
                                   (values `(,tmps ... ,be ...) `(,u ,e)))]
          [(,[bb b] ,[bb* b*] ...)
           (values `(,bb ... ,bb* ... ...) `(,b ,b* ...))]
          [() (values '() '())]
          [,e (errorf who "convert-bindings ~s" e)])))

    (define convert-literals
      (lambda (b)
        (cond
          [(null? b)
           (values '() '())]
          [(atom? b)
           (values '() b)]
          [(pair? (car b))
           (let-values ([(a b) (convert-literals (car b))]
                        [(c d) (convert-literals (cdr b))])
             (values `(,@a ,@c) (cons b d)))]
          [(and (eq? (car b) 'quote) (immediate? (cadr b))) (values '() b)]
          [(match b
             [(quote ,i) #t]
             [,_ #f])
           (let-values ([(tmp) (unique-name 'tmp)]
                        [(ba a) (Expr b)])
             (values `(,@ba (,tmp ,a)) tmp))]
          [else
            (let-values ([(bd d) (convert-literals (cdr b))])
              (values bd (cons (car b) d)))])))

    (define Expr
      (lambda (program)
        (match program
          [(false) (values '() '(false))]
          [(true)  (values '() '(true))]
          [(quote ,i) (guard (immediate? i)) (values '() `(quote ,i))]
          [(quote ,datum) (values '() (handle-complex-datum `(quote ,datum)))]
          [(if ,[bp p] ,[bc c] ,[ba a]) (values `(,bp ... ,bc ... ,ba ...)`(if ,p ,c ,a))]
          [(begin ,[be* e*] ... ,[bp p]) (values `(,be* ... ... ,bp ...) (make-begin `(,e* ... ,p)))]
          [(let (,bindings* ...) ,[bexpr expr])
           (let-values ([(b e) (convert-bindings bindings*)])
             (values `(,b ... ,bexpr ...) `(let ,e ,expr)))]
          [(letrec (,bindings* ...) ,[bexpr expr])
           (let-values ([(b e) (convert-bindings bindings*)])
             (values `(,b ...  ,bexpr ...) `(letrec ,e ,expr)))]
          [(lambda (,uvar* ...) ,[bexpr expr]) (values bexpr `(lambda ,uvar* ,expr))]
          [(set! ,uvar ,e)
           (let-values ([(bexpr expr) (Expr e)]
                        [(tmp) (unique-name 'tmp)])
             (if (complex-datum? e)
               (values `(,bexpr ... (,tmp ,expr)) `(set! ,uvar ,tmp))
               (values bexpr `(set! ,uvar ,expr))))]
          [(,prim ,[bv* v*] ...) (guard (prim? prim)) (values `(,bv* ... ...) `(,prim ,v* ...))]
          [,x (guard (ort x uvar? label? immediate? int64?)) (values '()  x)]
          [(,[bexpr expr] ,[bexpr* expr*] ...) (values  `(,bexpr ... ,bexpr* ... ...) `(,expr ,expr* ...))]
          [,e (errorf who "Expr ~s" e)])))


  (define convert-complex-datum
    (lambda (program)
      (match program
        [,[Expr -> blocks prog]
          (if (andmap null? blocks)
            prog
            `(let ,(if (list? (car blocks)) blocks `(,blocks)) ,prog))]
        [,e (errorf who "Program ~s" e)])))

  );; end lib
