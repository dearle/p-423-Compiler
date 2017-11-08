(library (Compiler remove-anonymous-lambda)
  (export remove-anonymous-lambda)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers))

(define-who remove-anonymous-lambda
  
  (define primitives
    '(+ - * <= < = >= > boolean? car cdr cons eq? fixnum? make-vector null? pair? procedure?
        set-car! set-cdr! vector? vector-length vector-ref vector-set! void))

  (define Expr
      (lambda (inlet?)
        (lambda (x)
        (match x
          [,uvar (guard (uvar? uvar)) uvar]
          [(quote ,x) `(quote ,x)]
          [(if ,[(Expr '#f) -> test] ,[(Expr '#f) -> conseq] ,[(Expr '#f) -> altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[(Expr '#f) -> ef*] ... ,[(Expr '#f) -> ef]) `(begin ,ef* ... ,ef)]
          [(lambda (,uvar* ...) ,[(Expr '#f) -> exp])
           (if inlet?
               `(lambda (,uvar* ...) ,exp)
               (let ([anon (unique-name 'anon)])
                 `(letrec ([,anon (lambda (,uvar* ...) ,exp)]) ,anon)))]
          [(let ([,new-uvar* ,[(Expr '#t) -> exp*]] ...) ,[(Expr '#f) -> exp])
           `(let ([,new-uvar* ,exp*] ...) ,exp)]
          [(letrec ([,new-uvar* ,[(Expr '#t) -> lambda*]] ...) ,[(Expr '#f) -> exp])
           `(letrec ([,new-uvar* ,lambda*] ...) ,exp)]
          [(,prim ,[(Expr '#f) -> x*] ...) (guard (memq prim primitives))
                                     `(,prim ,x* ...)]
          [(,[(Expr '#f) -> exp] ,[(Expr '#f) -> exp*] ...) `(,exp ,exp* ...)]
          [,x (error who "invalid Expr ~s" x)]))))
  
  (lambda (x)
    ((Expr '#f) x)))
)