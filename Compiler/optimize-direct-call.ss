(library (Compiler optimize-direct-call)
  (export optimize-direct-call)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers))

(define-who optimize-direct-call
  
  (define primitives
    '(+ - * <= < = >= > boolean? car cdr cons eq? fixnum? make-vector null? pair? procedure?
        set-car! set-cdr! vector? vector-length vector-ref vector-set! void))

  (define Expr
      (lambda (x)
        (match x
          [,uvar (guard (uvar? uvar)) uvar]
          [(quote ,x) `(quote ,x)]
          [(if ,[Expr -> test] ,[Expr -> conseq] ,[Expr -> altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[Expr -> ef*] ... ,[Expr -> ef]) `(begin ,ef* ... ,ef)]
          [(lambda (,uvar* ...) ,[Expr -> exp])
           `(lambda (,uvar* ...) ,exp)]
          [(let ([,new-uvar* ,[Expr -> exp*]] ...) ,[Expr -> exp])
           `(let ([,new-uvar* ,exp*] ...) ,exp)]
          [(letrec ([,new-uvar* ,[Expr -> lambda*]] ...) ,[Expr -> exp])
           `(letrec ([,new-uvar* ,lambda*] ...) ,exp)]
          [(,prim ,[Expr -> x*] ...) (guard (memq prim primitives))
                                     `(,prim ,x* ...)]
          [((lambda (,uvar* ...) ,[Expr -> exp]) ,[Expr -> exp*] ...)
           (if (eq? (length uvar*) (length exp*))
               `(let ([,uvar* ,exp*] ...) ,exp)
               `(lambda (,uvar* ...) ,exp))]
          [(,[Expr -> exp] ,[Expr -> exp*] ...) `(,exp ,exp* ...)]
          [,x (error who "invalid Expr ~s" x)])))
  
  (lambda (x)
    (Expr x)))
)

