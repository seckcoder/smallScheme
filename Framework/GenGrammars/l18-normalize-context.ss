;; Automatically generated file -- DO NOT MODIFY
(library (Framework GenGrammars l18-normalize-context)
  (export verify-grammar:l18-normalize-context)
  (import (chezscheme) (Framework match) (Framework prims))
  (define (any . nested-bool-ls)
    (letrec ([helper (lambda (x)
                       (cond
                         [(not x) #f]
                         [(null? x) #f]
                         [(pair? x) (or (helper (car x)) (helper (cdr x)))]
                         [else x]))])
      (helper nested-bool-ls)))
  (define verify-grammar:l18-normalize-context
    (lambda (x)
      (define Prog
        (lambda (x)
          (match x
            [(letrec ([,(Label -> x1) (lambda (,(UVar -> x2) ...)
                                        ,(Value -> x3))]
                      ...)
               ,(Value -> x4))
             (any x4 x3 x2 x1)]
            [,e (invalid-expr 'Prog e)])))
      (define Pred
        (lambda (x)
          (match x
            [(let ([,(UVar -> x1) ,(Value -> x2)] ...) ,(Pred -> x3))
             (any x3 x2 x1)]
            [(true) (any)]
            [(false) (any)]
            [(if ,(Pred -> x1) ,(Pred -> x2) ,(Pred -> x3))
             (any x3 x2 x1)]
            [(begin ,(Effect -> x1) ... ,(Pred -> x2)) (any x2 x1)]
            [(,(PredPrim -> x1) ,(Value -> x2) ...) (any x2 x1)]
            [,e (invalid-expr 'Pred e)])))
      (define Effect
        (lambda (x)
          (match x
            [(let ([,(UVar -> x1) ,(Value -> x2)] ...) ,(Effect -> x3))
             (any x3 x2 x1)]
            [(nop) (any)]
            [(if ,(Pred -> x1) ,(Effect -> x2) ,(Effect -> x3))
             (any x3 x2 x1)]
            [(begin ,(Effect -> x1) ... ,(Effect -> x2)) (any x2 x1)]
            [(,(EffectPrim -> x1) ,(Value -> x2) ...) (any x2 x1)]
            [(,(Value -> x1) ,(Value -> x2) ...) (any x2 x1)]
            [,e (invalid-expr 'Effect e)])))
      (define Value
        (lambda (x)
          (match x
            [,e (guard (not [UVar e])) #f]
            [,e (guard (not [Label e])) #f]
            [',(Immediate -> x1) (any x1)]
            [(let ([,(UVar -> x1) ,(Value -> x2)] ...) ,(Value -> x3))
             (any x3 x2 x1)]
            [(if ,(Pred -> x1) ,(Value -> x2) ,(Value -> x3))
             (any x3 x2 x1)]
            [(begin ,(Effect -> x1) ... ,(Value -> x2)) (any x2 x1)]
            [(,(ValPrim -> x1) ,(Value -> x2) ...) (any x2 x1)]
            [(,(Value -> x1) ,(Value -> x2) ...) (any x2 x1)]
            [,e (invalid-expr 'Value e)])))
      (let ([res (Prog x)])
        (if res
            (errorf 'verify-grammar:l18-normalize-context "~a" res)
            x)))))
