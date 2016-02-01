;; Automatically generated file -- DO NOT MODIFY
(library (Framework GenGrammars l07-remove-anonymous-lambda)
  (export verify-grammar:l07-remove-anonymous-lambda)
  (import (chezscheme) (Framework match) (Framework prims))
  (define (any . nested-bool-ls)
    (letrec ([helper (lambda (x)
                       (cond
                         [(not x) #f]
                         [(null? x) #f]
                         [(pair? x) (or (helper (car x)) (helper (cdr x)))]
                         [else x]))])
      (helper nested-bool-ls)))
  (define verify-grammar:l07-remove-anonymous-lambda
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
            [(letrec ([,(UVar -> x1) ,(Lamb -> x2)] ...) ,(Expr -> x3))
             (any x3 x2 x1)]
            [(let ([,(UVar -> x1) ,(LambdaOrExpr -> x2)] ...)
               ,(Expr -> x3))
             (any x3 x2 x1)]
            [,e (invalid-expr 'Expr e)])))
      (define Lamb
        (lambda (x)
          (match x
            [(lambda (,(UVar -> x1) ...) ,(Expr -> x2)) (any x2 x1)]
            [,e (invalid-expr 'Lamb e)])))
      (define LambdaOrExpr
        (lambda (x)
          (match x
            [,e (guard (not [Lamb e])) #f]
            [,e (guard (not [Expr e])) #f]
            [,e (invalid-expr 'LambdaOrExpr e)])))
      (let ([res (Prog x)])
        (if res
            (errorf 'verify-grammar:l07-remove-anonymous-lambda
              "~a"
              res)
            x)))))
