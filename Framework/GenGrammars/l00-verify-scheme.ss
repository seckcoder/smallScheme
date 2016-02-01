;; Automatically generated file -- DO NOT MODIFY
(library (Framework GenGrammars l00-verify-scheme)
  (export verify-grammar:l00-verify-scheme)
  (import (chezscheme) (Framework match) (Framework prims))
  (define (any . nested-bool-ls)
    (letrec ([helper (lambda (x)
                       (cond
                         [(not x) #f]
                         [(null? x) #f]
                         [(pair? x) (or (helper (car x)) (helper (cdr x)))]
                         [else x]))])
      (helper nested-bool-ls)))
  (define verify-grammar:l00-verify-scheme
    (lambda (x)
      (define Prog
        (lambda (x)
          (match x
            [,e (guard (not [Expr e])) #f]
            [,e (invalid-expr 'Prog e)])))
      (define Expr
        (lambda (x)
          (match x
            [,e (guard (not [Immediate e])) #f]
            [,e (guard (not [UVar e])) #f]
            [',(Datum -> x1) (any x1)]
            [(let ([,(UVar -> x1) ,(Expr -> x2)] ...) ,(Body -> x3) ...)
             (any x3 x2 x1)]
            [(letrec ([,(UVar -> x1) ,(Expr -> x2)] ...)
               ,(Body -> x3)
               ...)
             (any x3 x2 x1)]
            [(lambda (,(UVar -> x1) ...) ,(Body -> x2) ...) (any x2 x1)]
            [(and ,(Expr -> x1) ...) (any x1)]
            [(or ,(Expr -> x1) ...) (any x1)]
            [(not ,(Expr -> x1)) (any x1)]
            [(if . ,bod)
             (and (match (cons 'if bod)
                    [(if ,(Expr -> x1) ,(Expr -> x2)) (any x2 x1)]
                    [,e (invalid-expr 'if e)])
                  (match (cons 'if bod)
                    [(if ,(Expr -> x1) ,(Expr -> x2) ,(Expr -> x3))
                     (any x3 x2 x1)]
                    [,e (invalid-expr 'if e)]))]
            [(begin ,(Expr -> x1) ... ,(Expr -> x2)) (any x2 x1)]
            [(set! ,(UVar -> x1) ,(Expr -> x2)) (any x2 x1)]
            [(,(ValPrim -> x1) ,(Expr -> x2) ...) (any x2 x1)]
            [(,(EffectPrim -> x1) ,(Expr -> x2) ...) (any x2 x1)]
            [(,(PredPrim -> x1) ,(Expr -> x2) ...) (any x2 x1)]
            [(,(Expr -> x1) ,(Expr -> x2) ...) (any x2 x1)]
            [,e (invalid-expr 'Expr e)])))
      (define Body
        (lambda (x)
          (match x
            [,e (guard (not [Expr e])) #f]
            [,e (invalid-expr 'Body e)])))
      (let ([res (Prog x)])
        (if res
            (errorf 'verify-grammar:l00-verify-scheme "~a" res)
            x)))))
