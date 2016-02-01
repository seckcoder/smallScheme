(library
  (Compiler expose-frame-var)
  (export expose-frame-var)
  (import 
   (chezscheme)
   (Compiler my-helpers)
   (Framework helpers)
   (Framework match))

  (define who 'expose-frame-var)
  
  (define Triv
    (lambda (fvar offset)
    (make-disp-opnd 
      frame-pointer-register 
      (- (ash (frame-var->index fvar) word-shift) offset))))

(define expose-frame-var 
    (let ((offset 0))
    (lambda (program)
      (match program
        [(set! ,fp (+ ,fp ,shift)) 
         (guard (eq? fp frame-pointer-register))
         (set! offset shift)
         `(set! ,fp (+ ,fp ,shift))]
        [(set! ,fp (- ,fp ,shift)) 
         (guard (eq? fp frame-pointer-register)) 
         (set! offset (- offset shift))
         `(set! ,fp (- ,fp ,shift))]
        [(,a . ,d) ;; Need to execute serially, otherwise we mess up our offsets.
         (let* ([a (expose-frame-var a)]
                [d (expose-frame-var d)])
           `(,a . ,d))]
        [,fv (guard (frame-var? fv)) (Triv fv offset)]
        [,_ _]))))


  );; end lib
