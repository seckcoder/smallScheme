(library 
  (Compiler generate-x86-64)
  (export generate-x86-64)
  (import 
    (chezscheme)
    (Compiler my-helpers)
    (Framework helpers)
    (Framework match))

  ;; The goal of this pass is to take in an 'assembly like' 
  ;; version of scheme and to convert that expression into 
  ;; Intel X86-64 assembly. 
  
  (define who 'generate-x86-64)

  ;; Given a binop (+ - * logand logor sra),
  ;; this converts it to the equivalent assembly 
  ;; instruction given by the relation:
  ;; +      -> addq
  ;; -      -> subq
  ;; *      -> imulq
  ;; logand -> andq
  ;; logor  -> orq 
  ;; sra    -> sarq
  (define bin->xop
    (lambda (bin)
      (cdr (assq bin 
                 '((+ . addq) (- . subq) (* . imulq) 
                   (logand . andq) (logor . orq) (sra . sarq))))))

;; Given a relop (= < <= > >=) we return. (If the relop is negated the return is on
;; the right of |)
;; =  -> je | jne
;; <  -> jl | jge
;; <= -> jle | jg
;; >  -> jg | jle
;; >= -> jge | jl
  (define rel->xop
    (curry (negated? rel)  
      (let ((rel-list (if negated?
                        '((= . jne) (< . jge) (<= . jg) (> . jle) (>= . jl))
                        '((= . je) (< . jl) (<= . jle) (> . jg) (>= . jge)))))
      (cdr (assq rel rel-list)))))

  ;; We lop off the "code", emit an entry statement
  ;; so that the runtime system can recognize it, emit our code and
  ;; then emit an exit statement. We use a for-each as opposed to cata's,
  ;; since the order in which we emit the instructions is important.
  (define generate-x86-64
    (lambda (prog)
      (match prog
        [(code ,st* ...)
         (emit-program (for-each Code st*))]
        [,else (errorf who "Invalid input to genX86 ~s" else)])))

  ;; We take in a statement and emit the equivalent assembly instruction
  (define Code 
    (lambda (prog)
      (match prog
        [,lab (guard (label? lab)) (emit-label lab)]
        [(jump ,lab) (emit-jump 'jmp lab)]
        [(set! ,rand1 ,lab)
         (guard (label? lab))
         (emit 'leaq 
               lab 
               rand1)]
        [(set! ,dst (,binop ,dst ,src))
         (emit (bin->xop binop)
               src 
               dst)]
        [(set! ,dst ,src)
         (emit 'movq
               src
               dst)]
        [(if (not (,[(rel->xop #t) -> rop] ,c1 ,c2)) (jump ,lab))
         (emit 'cmpq c2 c1)
         (emit-jump rop lab)]
        [(if (,[(rel->xop #f) -> rop] ,c1 ,c2) (jump ,lab))
         (emit 'cmpq c2 c1)
         (emit-jump rop lab)]
        [,else (errorf who "Bad Statement ~s" else)])))
  ) ;; end lib
