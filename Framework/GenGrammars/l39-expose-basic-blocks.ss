;; Automatically generated file -- DO NOT MODIFY
(library (Framework GenGrammars l39-expose-basic-blocks)
  (export verify-grammar:l39-expose-basic-blocks)
  (import (chezscheme) (Framework match) (Framework prims))
  (define (any . nested-bool-ls)
    (letrec ([helper (lambda (x)
                       (cond
                         [(not x) #f]
                         [(null? x) #f]
                         [(pair? x) (or (helper (car x)) (helper (cdr x)))]
                         [else x]))])
      (helper nested-bool-ls)))
  (define verify-grammar:l39-expose-basic-blocks
    (lambda (x)
      (define Tail
        (lambda (x)
          (match x
            [(begin ,(Effect -> x1) ... ,(Tail -> x2)) (any x2 x1)]
            [(,(Triv -> x1)) (any x1)]
            [(if (,(Relop -> x1) ,(Triv -> x2) ,(Triv -> x3))
                 (,(Label -> x4))
                 (,(Label -> x5)))
             (any x5 x4 x3 x2 x1)]
            [,e (invalid-expr 'Tail e)])))
      (define Effect
        (lambda (x)
          (match x
            [(set! . ,bod)
             (and (match (cons 'set! bod)
                    [(set! ,(Loc -> x1) ,(Triv -> x2)) (any x2 x1)]
                    [,e (invalid-expr 'set! e)])
                  (match (cons 'set! bod)
                    [(set! ,(Loc -> x1)
                       (,(Binop -> x2) ,(Triv -> x3) ,(Triv -> x4)))
                     (any x4 x3 x2 x1)]
                    [,e (invalid-expr 'set! e)]))]
            [,e (invalid-expr 'Effect e)])))
      (define Triv
        (lambda (x)
          (match x
            [,e (guard (not [Integer e])) #f]
            [,e (guard (not [Label e])) #f]
            [,e (guard (not [Loc e])) #f]
            [,e (invalid-expr 'Triv e)])))
      (define Prog
        (lambda (x)
          (match x
            [(letrec ([,(Label -> x1) (lambda () ,(Tail -> x2))] ...)
               ,(Tail -> x3))
             (any x3 x2 x1)]
            [,e (invalid-expr 'Prog e)])))
      (define Loc
        (lambda (x)
          (match x
            [,e (guard (not [Reg e])) #f]
            [,e (guard (not [Disp e])) #f]
            [,e (guard (not [Ind e])) #f]
            [,e (invalid-expr 'Loc e)])))
      (let ([res (Prog x)])
        (if res
            (errorf 'verify-grammar:l39-expose-basic-blocks "~a" res)
            x)))))
