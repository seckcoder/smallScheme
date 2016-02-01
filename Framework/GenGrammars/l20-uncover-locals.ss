;; Automatically generated file -- DO NOT MODIFY
(library (Framework GenGrammars l20-uncover-locals)
  (export verify-grammar:l20-uncover-locals)
  (import (chezscheme) (Framework match) (Framework prims))
  (define (any . nested-bool-ls)
    (letrec ([helper (lambda (x)
                       (cond
                         [(not x) #f]
                         [(null? x) #f]
                         [(pair? x) (or (helper (car x)) (helper (cdr x)))]
                         [else x]))])
      (helper nested-bool-ls)))
  (define verify-grammar:l20-uncover-locals
    (lambda (x)
      (define Tail
        (lambda (x)
          (match x
            [,e (guard (not [Triv e])) #f]
            [(let ([,(UVar -> x1) ,(Value -> x2)] ...) ,(Tail -> x3))
             (any x3 x2 x1)]
            [(if ,(Pred -> x1) ,(Tail -> x2) ,(Tail -> x3))
             (any x3 x2 x1)]
            [(begin ,(Effect -> x1) ... ,(Tail -> x2)) (any x2 x1)]
            [(alloc ,(Value -> x1)) (any x1)]
            [(mref ,(Value -> x1) ,(Value -> x2)) (any x2 x1)]
            [(,(Binop -> x1) ,(Value -> x2) ,(Value -> x3))
             (any x3 x2 x1)]
            [(,(Value -> x1) ,(Value -> x2) ...) (any x2 x1)]
            [,e (invalid-expr 'Tail e)])))
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
            [(,(Relop -> x1) ,(Value -> x2) ,(Value -> x3))
             (any x3 x2 x1)]
            [,e (invalid-expr 'Pred e)])))
      (define Effect
        (lambda (x)
          (match x
            [(let ([,(UVar -> x1) ,(Value -> x2)] ...) ,(Effect -> x3))
             (any x3 x2 x1)]
            [(nop) (any)]
            [(mset! ,(Value -> x1) ,(Value -> x2) ,(Value -> x3))
             (any x3 x2 x1)]
            [(if ,(Pred -> x1) ,(Effect -> x2) ,(Effect -> x3))
             (any x3 x2 x1)]
            [(begin ,(Effect -> x1) ... ,(Effect -> x2)) (any x2 x1)]
            [(,(Value -> x1) ,(Value -> x2) ...) (any x2 x1)]
            [,e (invalid-expr 'Effect e)])))
      (define Value
        (lambda (x)
          (match x
            [,e (guard (not [Triv e])) #f]
            [(let ([,(UVar -> x1) ,(Value -> x2)] ...) ,(Value -> x3))
             (any x3 x2 x1)]
            [(if ,(Pred -> x1) ,(Value -> x2) ,(Value -> x3))
             (any x3 x2 x1)]
            [(begin ,(Effect -> x1) ... ,(Value -> x2)) (any x2 x1)]
            [(alloc ,(Value -> x1)) (any x1)]
            [(mref ,(Value -> x1) ,(Value -> x2)) (any x2 x1)]
            [(,(Binop -> x1) ,(Value -> x2) ,(Value -> x3))
             (any x3 x2 x1)]
            [(,(Value -> x1) ,(Value -> x2) ...) (any x2 x1)]
            [,e (invalid-expr 'Value e)])))
      (define Triv
        (lambda (x)
          (match x
            [,e (guard (not [UVar e])) #f]
            [,e (guard (not [Integer e])) #f]
            [,e (guard (not [Label e])) #f]
            [,e (invalid-expr 'Triv e)])))
      (define Prog
        (lambda (x)
          (match x
            [(letrec ([,(Label -> x1) (lambda (,(UVar -> x2) ...)
                                        ,(Body -> x3))]
                      ...)
               ,(Body -> x4))
             (any x4 x3 x2 x1)]
            [,e (invalid-expr 'Prog e)])))
      (define Body
        (lambda (x)
          (match x
            [(locals (,(UVar -> x1) ...) ,(Tail -> x2)) (any x2 x1)]
            [,e (invalid-expr 'Body e)])))
      (let ([res (Prog x)])
        (if res
            (errorf 'verify-grammar:l20-uncover-locals "~a" res)
            x)))))
