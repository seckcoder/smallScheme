;; Automatically generated file -- DO NOT MODIFY
(library (Framework GenGrammars l01-parse-scheme)
  (export verify-grammar:l01-parse-scheme)
  (import (chezscheme) (Framework match) (Framework prims))
  (define (any . nested-bool-ls)
    (letrec ([helper (lambda (x)
                       (cond
                         [(not x) #f]
                         [(null? x) #f]
                         [(pair? x) (or (helper (car x)) (helper (cdr x)))]
                         [else x]))])
      (helper nested-bool-ls)))
  (define verify-grammar:l01-parse-scheme
    (lambda (x)
      (define Prog
        (lambda (x)
          (match x
            [,e (guard (not [Expr e])) #f]
            [,e (invalid-expr 'Prog e)])))
      (define Body
        (lambda (x)
          (match x
            [,e (guard (not [Expr e])) #f]
            [,e (invalid-expr 'Body e)])))
      (define Expr
        (lambda (x)
          (match x
            [,e (guard (not [UVar e])) #f]
            [',(Immediate -> x1) (any x1)]
            [(let ([,(UVar -> x1) ,(Expr -> x2)] ...) ,(Body -> x3))
             (any x3 x2 x1)]
            [(letrec ([,(UVar -> x1) ,(Expr -> x2)] ...) ,(Body -> x3))
             (any x3 x2 x1)]
            [(lambda (,(UVar -> x1) ...) ,(Body -> x2)) (any x2 x1)]
            [(if ,(Expr -> x1) ,(Expr -> x2) ,(Expr -> x3))
             (any x3 x2 x1)]
            [(begin ,(Expr -> x1) ... ,(Expr -> x2)) (any x2 x1)]
            [(set! ,(UVar -> x1) ,(Expr -> x2)) (any x2 x1)]
            [(,(ValPrim -> x1) ,(Expr -> x2) ...) (any x2 x1)]
            [(,(EffectPrim -> x1) ,(Expr -> x2) ...) (any x2 x1)]
            [(,(PredPrim -> x1) ,(Expr -> x2) ...) (any x2 x1)]
            [(,(Expr -> x1) ,(Expr -> x2) ...) (any x2 x1)]
            [,e (invalid-expr 'Expr e)])))
      (let ([res (Prog x)])
        (if res
            (errorf 'verify-grammar:l01-parse-scheme "~a" res)
            x)))))
