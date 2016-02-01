(library 
  (Compiler expose-memory-operands)
  (export expose-memory-operands)
  (import 
    (Compiler my-helpers)
    (Framework helpers)
    (Framework match)
    (chezscheme))
  
  (define gen-disp-opnd 
    (lambda (fvar offset)
    (make-disp-opnd frame-pointer-register fvar offset)))
  
  (define-who expose-memory-operands 
    (define fp-offset 0)
    (lambda (program)
      (match program
        [(mset! ,reg ,index ,t3) (guard (register? reg) (register? index))
        `(set! ,(make-index-opnd reg index) ,t3)]
        [(mset! ,reg ,index ,t3) 
        `(set! ,(make-disp-opnd reg index) ,t3)]
        [(set! ,var (mref ,reg ,index)) (guard (register? reg) (register? index))
         `(set! ,var ,(make-index-opnd reg index))]
        [(set! ,var (mref ,reg ,index))
         `(set! ,var ,(make-disp-opnd reg index))]
        [(,a . ,d) 
         ;;the * forces serial execution of the lhs before the rhs, without it, bad things happen
         (let* ([lhs (expose-memory-operands a)]
                [rhs (expose-memory-operands d)])
           `(,lhs . ,rhs))]
        [,fv (guard (frame-var? fv)) (gen-disp-opnd fv fp-offset)]
        [,_ _])))





  ) ;; end lib
