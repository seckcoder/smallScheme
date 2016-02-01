;; Automatically generated file -- DO NOT MODIFY
(library (Framework GenGrammars l09-uncover-free)
  (export verify-grammar:l09-uncover-free)
  (import (chezscheme) (Framework match) (Framework prims))
  (define (any . nested-bool-ls)
    (letrec ([helper (lambda (x)
                       (cond
                         [(not x) #f]
                         [(null? x) #f]
                         [(pair? x) (or (helper (car x)) (helper (cdr x)))]
                         [else x]))])
      (helper nested-bool-ls)))
  (define verify-grammar:l09-uncover-free
    (lambda (x)
      (define Prog
        (lambda (x)
          (match x
            [,e (guard (not [Expr e])) #f]
            [,e (invalid-expr 'Prog e)])))
      (define Expr
        (lambda (x)
          (match x
            [,e (guard (not [UVar e])) #f]
            [(if ,(Expr -> x1) ,(Expr -> x2) ,(Expr -> x3))
             (any x3 x2 x1)]
            [(begin ,(Expr -> x1) ... ,(Expr -> x2)) (any x2 x1)]
            [(,(ValPrim -> x1) ,(Expr -> x2) ...) (any x2 x1)]
            [(,(EffectPrim -> x1) ,(Expr -> x2) ...) (any x2 x1)]
            [(,(PredPrim -> x1) ,(Expr -> x2) ...) (any x2 x1)]
            [(,(Expr -> x1) ,(Expr -> x2) ...) (any x2 x1)]
            [',(Immediate -> x1) (any x1)]
            [(let ([,(UVar -> x1) ,(Expr -> x2)] ...) ,(Expr -> x3))
             (any x3 x2 x1)]
            [(letrec ([,(UVar -> x1) (lambda (,(UVar -> x2) ...)
                                       (free
                                         (,(UVar -> x3) ...)
                                         ,(Expr -> x4)))]
                      ...)
               ,(Expr -> x5))
             (any x5 x4 x3 x2 x1)]
            [,e (invalid-expr 'Expr e)])))
      (let ([res (Prog x)])
        (if res
            (errorf 'verify-grammar:l09-uncover-free "~a" res)
            x)))))
