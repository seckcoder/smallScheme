(library 
  (Compiler select-instructions)
  (export select-instructions)
  (import 
    (Compiler my-helpers)
    (Framework match)
    (Framework helpers)
    (chezscheme))

;; The idea behind select-instructions is that we no longer check 
;; machine constraints in verify-scheme, and we now do this here. 
;; However, we now no longer just error out, but rather try to
;; remedy this problem via correctness preserving transformations given by:
;; (These were taken from the lecture notes)

;;        ************ (set! Var Triv) **************
;;          | Uvar/Reg | fvar | int32 | int64/label | (Triv)
;;          |          |      |       |             |
;; Uvar/Reg |   OK     |  OK  |   OK  |     OK      |
;; ---------+----------+------+-------+-------------+
;;          |          |      |       |             | 
;; fvar     |   OK     |  X   |   OK  |     X       |
;; ---------+----------+------+-------+-------------+
;; (Var)
;;
;; We fix the X's by introducing an unspillable u, and changing 
;; (set! Var Triv) ⇒ (begin (set! u Triv) (set! Var u))

;; ******* (set! Var (binop Triv1 Triv2)) *******
;;
;; BINOP, Case 1: (AKA Binop1)
;; ******* (set! Var (binop Triv1 Triv2)) *******
;; * If Var = Triv1
;;   - Go to Binop2
;; * If Binop is commutativ & Var = Triv2
;;   - Swap Triv1 and Triv2 and then go to Binop2
;; * ELSE Create an unspillable u and create:
;;   `(begin (set! u Triv1)
;;     ,(Binop2 (set! u (binop u Triv2)))
;;      (set! Var u))
;;      
;; BINOP, Case 2: (AKA Binop2)
;; ******* (set! Var (binop Var Triv)) *******
;;
;; SUBCASE1: binop ∈ {-, +, logor, logand,}
;;
;;          | Uvar/Reg | fvar | int32 | int64/label | (Triv)
;;          |          |      |       |             |
;; Uvar/Reg |   OK     |  OK  |   OK  |     X1      |
;; ---------+----------+------+-------+-------------+
;;          |          |      |       |             | 
;; fvar     |   OK     |  X1  |   OK  |     X1      |
;; ---------+----------+------+-------+-------------+
;; (Var)
;;
;; We fix the X1's by creating a new unspillable u and then doing:
;; (set! Var (binop Var Triv)) ⇒ (begin (set! u Triv) (set! Var (binop Var u)))
;;
;; SUBCASE2: binop = *:
;;
;;          | Uvar/Reg | fvar | int32 | int64/label | (Triv)
;;          |          |      |       |             |
;; Uvar/Reg |   OK     |  OK  |   OK  |     X1      |
;; ---------+----------+------+-------+-------------+
;;          |          |      |       |             | 
;; fvar     |   X2     |  X2  |   X2  |     X2      |
;; ---------+----------+------+-------+-------------+
;; (Var)
;;
;; We fix the X2's by introducing an unspillable u, and then doing:
;; (set! Var (binop Var Triv)) ⇒ 
;;  `(begin (set! u Var) 
;;      ,(Binop2 (set! u (binop u Triv)))
;;      (set! Var u))
;;
;; SUBCASE3: binop = sra:
;; This always succeeds since an earlier pass (verify-scheme) forced Triv2 to be an int

;; ************ (relop Triv1 Triv2) *************
;; Relop:
;; * If Triv1 ∈ {Uvar, Reg, Fvar}
;;   - (Relop2 (relop Triv1 Triv2))
;; * If Triv1 not in {Uvar, Reg, Fvar}, Triv2 ∈ {Uvar,Reg, Fvar}
;;   - (Relop2 (relop^ Triv2 Triv1))
;; * ELSE
;;   `(begin
;;     (set! u Triv1)
;;     ,(Relop2 (relop u Triv2)))
;; Where
;;  relop^ ≔ < ⇒ >
;;         | > ⇒ <
;;         | <= ⇒ >=
;;         | >= ⇒ <=
;;         | = ⇒ =
;;
;;Relop2: 
;;  ****** (relop Uvar/Reg/Fvar Triv2) ******
;;          | Uvar/Reg | fvar | int32 | int64/label | (Triv)
;;          |          |      |       |             |
;; Uvar/Reg |   OK     |  OK  |   OK  |     X8      |
;; ---------+----------+------+-------+-------------+
;;          |          |      |       |             | 
;; fvar     |   OK     |  X8  |   OK  |     X8      |
;; ---------+----------+------+-------+-------------+
;; 
;; We fix the X8's by introducing an unspillable u and then doing:
;; (begin (set! u Triv2) (relop Uvar/Reg/Fvar u))

;; ************ (mset! Triv1 Triv2 Triv3) ***********
;;
;; make this a value-map
;; (define handle-mset!-1
;;  (lambda (t1 t2 t3)
;;    (map (lambda (x) 
;;           (if (ort x frame-var? label?)
;;             `(set! ,(new-uloc) ,x)
;;             x)) `(mset! ,t1 ,t2 t3))))

;;  ***** (set! UV (mref t1 t2)) *********  
;;
;;  CASE1: UV =/= fvar
;;
;;          | Uvar/Reg | fvar | int32 | int64/label | (t2)
;;          |          |      |       |             |
;; Uvar/Reg |    OK    |  OK  |   OK  |     X9      |
;; ---------+----------+------+-------+-------------+
;;          |          |      |       |             | 
;; fvar     |    X1    |  X9  |   X1  |     X9      |
;; ---------+----------+------+-------+-------------+
;;          |          |      |       |             | 
;; int      |    X1    |  X1  |   X1  |     X1      |
;; ---------+----------+------+-------+-------------+
;; (t1)
;;
;; If UV = fvar. Then we create a temp for UV and leave the rest unchanged


(define who 'select-instructions )

(define Body 
  (lambda (bd)

    (define new-locals '())

    (define new-uloc
      (lambda ()
        (let ((u (unique-name 'u))) 
          (set! new-locals (cons u new-locals))
          u)))

    (define regv?
      (lambda (v)
        (ort v register? uvar?)))

    (define relop->relop^
      (lambda (op)
        (cdr (assq op '((<= . >=) (= . =) (< . >) (> . <) (>= . <=))))))

    ;; FIXME!!  works but in-elegent
    (define select-mref
      (lambda (mref)
        (match mref
          [(set! ,uvar (mref ,t1 ,t2)) 
           (guard (or (label? t1) (frame-var? t1))
                  (or (frame-var? t2) (label? t2) (not (int64? t2)))) 
           (let ([temp1 (new-uloc)]
                 [temp2 (new-uloc)])
             (make-begin `((set! ,temp1 ,t1)
                                   (set! ,temp2 ,t2)
                                   (set! ,uvar (mref ,temp1 ,temp2)))))]
          [(set! ,uvar (mref ,t1 ,t2))
           (guard (frame-var? uvar) (not (frame-var? t1)) (or (int32? t2) (int64? t2)))
           (let ([temp (new-uloc)])
             (make-begin `((set! ,temp ,uvar)
                                   (set! ,temp (mref ,t1 ,t2))
                                   (set! ,uvar ,temp))))]
          [(set! ,uvar (mref ,t1 ,t2))
           (guard  (frame-var? uvar))
           (let ([temp (new-uloc)]
                 [temp1 (new-uloc)])
             (make-begin `((set! ,temp1 ,uvar)
                                   (set! ,temp ,t1)
                                   (set! ,temp1 (mref ,temp ,t2))
                                   (set! ,uvar ,temp1))))]
          [(set! ,uvar (mref ,t1 ,t2))
           (guard  (frame-var? t1) (or (int32? t2) (int64? t2)))
           (let ([temp (new-uloc)])
             (make-begin `((set! ,temp ,t1)
                                   (set! ,uvar (mref ,temp ,t2)))))]
          [(set! ,uvar (mref ,t1 ,t2)) 
           (guard (not (frame-var? uvar)) (not (int32? t2)) (int64? t2))
           (let ([temp (new-uloc)])
             (make-begin `((set! ,temp ,t2)
                                   (set! ,uvar (mref ,temp ,t1)))))]
          [(set! ,uvar (mref ,t1 ,t2)) 
           (guard (int32? t2) (not (frame-var? t1)))
          `(set! ,uvar (mref ,t1 ,t2))]
          [(set! ,uvar (mref ,t1 ,t2)) 
           (guard (frame-var? t1))
           (let ([temp (new-uloc)]) 
             (make-begin `((set! ,temp ,t1)
                                   (set! ,uvar (mref ,temp ,t2)))))]
          [(set! ,uvar (mref ,t1 ,t2)) 
           (guard (or (frame-var? t2) (label? t2)))
           (let ([temp (new-uloc)])
             (make-begin `((set! ,temp ,t2)
                                   (set! ,uvar (mref ,t1 ,temp)))))]
          [(set! ,uvar (mref ,t1 ,t2))
           (guard (integer? t1))
           (let ([temp (new-uloc)])
             (make-begin `((set! ,temp ,t1)
                                   (set! ,uvar (mref ,temp ,t2)))))]
          [(set! ,uvar (mref ,t1 ,t2)) 
           `(set! ,uvar (mref ,t1 ,t2))]
          )))


    ;; FIXME!!  works but in-elegent
    (define select-mset
      (lambda (e)
        (match e
          [(mset! ,t1 ,t2 ,t3) (guard (or (frame-var? t1) (label? t1))
                                      (or (frame-var? t2) (label? t2))
                                      (or (frame-var? t3) (label? t3)))
                               (let ([temp1 (new-uloc)]
                                     [temp2 (new-uloc)]
                                     [temp3 (new-uloc)])
                                 (make-begin `((set! ,temp1 ,t3)
                                               (set! ,temp2 ,t1)
                                               (set! ,temp3 ,t2)
                                               (mset! ,temp2 ,temp3 ,temp1))))]
          [(mset! ,t1 ,t2 ,t3) (guard (or (frame-var? t1) (label? t1)) (or (frame-var? t3) (label? t3)))
                               (let ([temp1 (new-uloc)]
                                     [temp2 (new-uloc)])
                                 (make-begin `((set! ,temp1 ,t3)
                                               (set! ,temp2 ,t1)
                                               (mset! ,temp2 ,t2 ,temp1))))]
          [(mset! ,t1 ,t2 ,t3) (guard (or (frame-var? t2) (label? t2))
                                      (or (frame-var? t1) (label? t1)))
                               (let ([temp1 (new-uloc)]
                                     [temp2 (new-uloc)])
                                 (make-begin `((set! ,temp1 ,t2)
                                               (set! ,temp2 ,t1)
                                               (mset! ,temp2 ,temp1 ,t3))))]
          [(mset! ,t1 ,t2 ,t3) (guard (or (frame-var? t2) (label? t2))
                                      (or (frame-var? t3) (label? t3)))
                               (let ([temp1 (new-uloc)]
                                     [temp2 (new-uloc)])
                                 (make-begin `((set! ,temp1 ,t3)
                                               (set! ,temp2 ,t2)
                                               (mset! ,t1 ,temp2 ,temp1))))]
          [(mset! ,t1 ,t2 ,t3) (guard (or (frame-var? t3) (label? t3)))
                               (let ([temp (new-uloc)])
                                 (make-begin `((set! ,temp ,t3)
                                               (mset! ,t1 ,t2 ,temp))))]
          [(mset! ,t1 ,t2 ,t3) (guard (or (frame-var? t2) (label? t2)))
                               (let ([temp (new-uloc)])
                                 (make-begin `((set! ,temp ,t2)
                                               (mset! ,t1 ,temp ,t3))))]
          [(mset! ,t1 ,t2 ,t3) (guard (or (frame-var? t1) (label? t1)))
                               (let ([temp (new-uloc)])
                                 (make-begin `((set! ,temp ,t1)
                                               (mset! ,temp ,t2 ,t3))))]
          ;;probably don't need this
          [(mset! ,t1 ,t2 ,t3) (guard (frame-var? t1) (int32?  t2) (int32? t3))
                               (let ([temp (new-uloc)])
                                 (make-begin `((set! ,temp ,t1)
                                               (mset! ,temp ,t2 ,t3))))]
          [(mset! ,t1 ,t2 ,t3) (guard (frame-var? t1) 
                                      (or (frame-var? t2) (int64? t2))
                                      (int32? t3)) 
                               (let ([temp1 (new-uloc)]
                                     [temp2 (new-uloc)])
                                 (make-begin `((set! ,temp1 ,t1)
                                               (set! ,temp2 ,t2)
                                               (mset! ,temp1 ,temp2 ,t3))))]
          [(mset! ,t1 ,t2 ,t3) (guard (frame-var? t1) 
                                      (or (frame-var? t2) (int64? t2))
                                      (or (frame-var? t3) (int64? t3))) 
                               (let ([temp1 (new-uloc)]
                                     [temp2 (new-uloc)]
                                     [temp3 (new-uloc)])
                                 (make-begin `((set! ,temp1 ,t1)
                                               (set! ,temp2 ,t2)
                                               (set! ,temp3 ,t3)
                                               (mset! ,temp1 ,temp2 ,temp3))))]
          [(mset! ,t1 ,t2 ,t3) (guard (or (frame-var? t1) (label? t1) (integer? t1)))
                               (let ([temp (new-uloc)])
                                 (make-begin `((set! ,temp ,t1)
                                               (mset! ,temp ,t2 ,t3))))]
          [(mset! ,t1 ,t2 ,t3)
           `(mset! ,t1 ,t2 ,t3)])))
    
;; Non-working previous versions....
    #;(define select-mref
    (lambda (uvar t1 t2)
      (cond
        [(frame-var? t1)
         (if (ort t2 frame-var? label? (lambda (x) (and (int64? x) (not (int32? x)))))
           (let ([temp1 (new-uloc)]
                 [temp2 (new-uloc)])
             (make-begin `((set! ,temp1 ,t1)
                           (set! ,temp2 ,t2)
                           (set! ,uvar (mref ,temp1 ,temp2)))))
           (let ([temp (new-uloc)]) ;; this is where one error was happening
             (make-begin `((set! ,temp ,t1)
                           (set! ,uvar (mref ,temp ,t2))))))]
        [(frame-var? uvar) 
         (let ([temp (new-uloc)])
           (make-begin `((set! ,temp ,uvar)
                         (set! ,temp (mref ,t1 ,t2))
                         (set! ,uvar ,temp))))]
        [(integer? t1)
         (let ([temp (new-uloc)])
           (make-begin `((set! ,temp ,t1)
                         (set! ,uvar (mref ,temp ,t2)))))]
        ;; might need to add in regv line
        [else `(set! ,uvar (mref ,t1 ,t2))])))   
;; NOn-working prev version
  #;(define select-mset
  (lambda x
    (let-values ([(sets binds)
                  (map-values 
                    (lambda (t)
                      (if (ort t frame-var? label? int64?)
                        (let ((tmp (new-uloc)))
                          (values `((set! ,tmp ,t)) tmp))
                        (values '() t)))
                    x)])
      (with-ellipsis-aware-quasiquote 
        (make-begin `(,sets ... ... (mset! ,@binds)))))))


(define select-move
  (lambda (var triv)
    (if (or (regv? var) (==> (var : frame-var?) (triv : regv? int32?)))
      `(set! ,var ,triv)
      (let ((u (new-uloc)))
        (make-begin `((set! ,u ,triv) (set! ,var ,u)))))))

(define select-binop-1
  (lambda (var op triv1 triv2)
    (cond
      [(eq? var triv1) (select-binop-2 var op triv2)]
      [(and (eq? triv2 var) (memq op '(* + logor logand))) 
       (select-binop-2 var op triv1)]
      [else (let ((u (new-uloc)))
              (make-begin
                `((set! ,u ,triv1)
                  ,(select-binop-2 u op triv2)
                  (set! ,var ,u))))])))

(define select-binop-2
  (lambda (var op triv)
    (cond
      [(memq op '(- + logand logor))
       (if (or (==> (var : regv?) (triv : regv? frame-var? int32?))
               (==> (var : frame-var?) (triv : regv? int32?)))
         `(set! ,var (,op ,var ,triv))
         (let ((u (new-uloc)))
           (make-begin `((set! ,u ,triv) (set! ,var (,op ,var ,u))))))]
      [(eq? op '*)
       (if (regv? var)
         (if (ort triv regv? frame-var? int32?)
           `(set! ,var (,op ,var ,triv))
           (let ((u (new-uloc)))
             (make-begin `((set! ,u ,triv) (set! ,var (,op ,var ,u))))))
         (let ((u (new-uloc)))
           (make-begin `((set! ,u ,var)
                         ,(select-binop-2 u op triv)
                         (set! ,var ,u)))))]
      [(eq? op 'sra) `(set! ,var (,op ,var ,triv))])))

(define select-relop
  (lambda (op triv1 triv2)
    (cond
      [(ort triv1 regv? frame-var?) (select-relop-2 op triv1 triv2)]
      [(ort triv2 regv? frame-var?) (select-relop-2 (relop->relop^ op) triv2 triv1)]
      [else (let ((u (new-uloc)))
              (make-begin `((set! ,u ,triv1)
                            ,(select-relop-2 op u triv2))))])))

;; U/R/F AKA Uvar/Reg/Fvar
(define select-relop-2
  (lambda (op U/R/F triv2)
    (cond
      [(==> (U/R/F : regv?) (triv2 : regv? frame-var? int32?)) `(,op ,U/R/F ,triv2)]
      [(ort triv2 regv? int32? ) `(,op ,U/R/F ,triv2)]
      [else (let ((u (new-uloc)))
              (make-begin `((set! ,u ,triv2) (,op ,U/R/F ,u))))]))) 


(define Pred
  (lambda (prd)
    (match prd
      [(true) '(true)]
      [(false) '(false)]
      [(if ,[p] ,[c] ,[a])
       `(if ,p ,c ,a)]
      [(begin ,(Effect -> eff*) ... ,(p)) (make-begin `(,eff* ... ,p))]
      [(,relop ,lhs ,rhs)
       (select-relop relop lhs rhs)]
      [,e (errorf who "Error with ~s" e)])))

(define Effect
  (lambda (effct)
    (match effct
      [(nop) '(nop)]
      [(mset! ,t1 ,t2 ,t3) (select-mset `(mset! ,t1 ,t2 ,t3))]
      [(set! ,uv (mref ,t1 ,t2)) (select-mref `(set! ,uv (mref ,t1 ,t2)))]
      [(set! ,lhs (,binop ,v1 ,v2)) (select-binop-1 lhs binop v1 v2)]
      [(set! ,lhs ,rhs) (select-move lhs rhs) ]
      [(if ,(Pred -> p) ,(c) ,(a)) `(if ,p ,c ,a)]
      [(begin ,(eff*) ... ,(ef)) (make-begin `(,eff* ... ,ef))]
      [(return-point ,lb ,[Tail -> tl]) `(return-point ,lb ,tl)]
      [,e (error who "Error with ~s" e)])))

(define Tail
  (lambda (effct)
    (match effct
      [(if ,(Pred -> p) ,(c) ,(a))
       `(if ,p ,c ,a)]
      [(begin ,(Effect -> eff*) ... ,(tl)) 
       (make-begin `(,eff* ... ,tl))]
      [(,t ,l ...) `(,t ,l ...)]
      [,e (errorf who "Error with ~s" e)])))

;; THIS WAS REALLY DEFINED ABOVE..... 
;(define Body
;(lambda (bd)
(match bd
  [(locals (,loc* ...)
     (ulocals (,uloc* ...)
       (locate (,assed ...) 
         (frame-conflict ,ct ,[Tail -> tail]))))
   `(locals (,loc* ...)
      (ulocals (,uloc* ... ,new-locals ...) 
        (locate (,assed ...)
          (frame-conflict ,ct ,tail))))]
  [(locate (,assed ...) ,tail) `(locate (,assed ...) ,tail)]
  [,x (errorf who "Error with ~s" x)])))

(define select-instructions
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,x (errorf who "Error in Program ~s" x)])))



)
