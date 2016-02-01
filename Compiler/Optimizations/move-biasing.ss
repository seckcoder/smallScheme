(library 
  (Compiler Optimizations move-biasing)
  (export bias-moves)
  (import 
    (Framework match)
    (Framework helpers)
    (Compiler my-helpers)
    (chezscheme))
  
  (define who 'move-biasing)

  #;(define get-bias
    (lambda (b blist fn)
      (match b
        [(begin ,ef* ... ,ef)
         (Effect* `(,ef* ... ,ef) blist fn)]
        [,e (errorf who "get-bias ~s" e)])))

(define Effect
  (lambda (fn)
    (lambda (eff* blist)
      (match eff*
        [(begin ,ef* ... ,[ef]) (fold-right (Effect fn) ef eff*)]
        [(set! ,x ,y) 
         (cond
           [(and (uvar? x) (uvar? y)) 
            (begin 
              (set-cdr! (assq x blist) (set-cons y (cdr (assq x blist))))
              (set-cdr! (assq y blist) (set-cons x (cdr (assq y blist))))
              blist)]
           [(and (uvar? x) (fn y))
            (begin 
              (set-cdr! (assq x blist) (set-cons y (cdr (assq x blist))))
              blist)]
           [(and (fn x) (uvar? y))
            (begin 
              (set-cdr! (assq y blist) (set-cons x (cdr (assq y blist))))
              blist)])]
        [,e blist]))))

(define bias-moves
  (lambda (tl fn uv*)
    (let* ((blist (map list uv*))
           (new-blist ((Effect fn) tl blist)))
      new-blist)))
  
  
  
  
  
  
  );; end lib
