(library
  (Compiler optimize-jumps)
  (export optimize-jumps)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers)
    (Compiler my-helpers))

  ;;we first generate an association list of labels in remove-trivial-jumps,
  ;;removing blocks containing labels that just jump to another label.
  ;;We then search and replace through the code to patch up the removed
  ;;labels with their walked labels.

  (define who 'optimize-jumps)

  (define remove-trivial-jumps
    (lambda (prog)
      (match prog
        [(,lab (lambda () (,jlab))) (guard (label? jlab)) (values `(,lab . ,jlab) '())]
        [,else (values '() `(,else))]))) 

  (define sandr
    (lambda (ls jumpls)
      (cond
        [(null? ls) '()]
        [(pair? (car ls)) (cons (sandr (car ls) jumpls) (sandr (cdr ls) jumpls))]
        [(assq (car ls) jumpls) => (lambda (lab) (cons (walk (cdr lab) jumpls) (sandr (cdr ls) jumpls)))]
        [else (cons (car ls) (sandr (cdr ls) jumpls))])))

  (define optimize-jumps
    (lambda (prog)
      (match prog
        [(letrec (,[remove-trivial-jumps -> j* b*] ...) ,b)
         (let* ([jumpls (fold-left (lambda (s x) (if (null? x) s (cons x s))) '() j*)])
           `(letrec (,(sandr b* jumpls) ... ...) ,(sandr b jumpls)))]
        [,e (errorf who "Optimize jumps ~s" e)])))



         (define walk
           (lambda (x ls)
             (match (assq x ls)
               [#f x]
               [(,t . ,h) (walk h ls)])))

  ) ;; end lib
