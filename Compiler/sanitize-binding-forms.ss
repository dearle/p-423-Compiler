(library (Compiler sanitize-binding-forms)
  (export sanitize-binding-forms)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers))

(define-who sanitize-binding-forms
  
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
        [(lambda (,uvar* ...) ,[Expr -> exp]) `(lambda (,uvar* ...) ,exp)]
        [(let ([,uvar* ,[Expr -> exp*]] ...) ,[Expr -> exp])
         (let-values ([(lam-binds let-binds) 
                       (partition (lambda (ls)
                                    (match ls
                                      [(,name (lambda (,bind* ...) ,body)) #t]
                                      [,x #f]))
                                  `([,uvar* ,exp*] ...))])
           (cond
             [(null? lam-binds) `(let (,let-binds ...) ,exp)]
             [(null? let-binds) `(letrec (,lam-binds ...) ,exp)]
             [else `(letrec (,lam-binds ...) (let (,let-binds ...) ,exp))]))]
        [(letrec ([,new-uvar* ,[Expr -> lambda*]] ...) ,[Expr -> exp])
         `(letrec ([,new-uvar* ,lambda*] ...) ,exp)]
        [(,prim ,[Expr -> x*] ...) (guard (memq prim primitives))
                                         `(,prim ,x* ...)]
        [(,[Expr -> exp] ,[Expr -> exp*] ...) `(,exp ,exp* ...)]
        [,x (error who "invalid Expr ~s" x)])))

(lambda (x)
  (Expr x)))
)