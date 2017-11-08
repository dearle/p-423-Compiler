(library (Compiler lift-letrec)
  (export lift-letrec)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers))

  (define-who lift-letrec
    
    (define primitives
      '(+ - * <= < = >= > boolean? car cdr cons eq? fixnum? make-vector null? 
          pair? set-car! set-cdr! vector? vector-length vector-ref vector-set! 
          procedure? make-procedure procedure-code procedure-ref procedure-set! void))
    
    (define Program
      (lambda (prog)
        
        (define top-rec '())
        
        (define Expr
        (lambda (expr)
          (match expr
            [,label (guard (label? label)) label]
            [,uvar (guard (uvar? uvar)) uvar]
            [(quote ,x) `(quote ,x)]
            [(if ,[Expr -> test] ,[Expr -> conseq] ,[Expr -> altern])
             `(if ,test ,conseq ,altern)]
            [(begin ,[Expr -> ef*] ... ,[Expr -> ef]) (make-begin `(,ef* ... ,ef))]
            [(let ([,var* ,[Expr -> exp*]] ...) ,[Expr -> exp])
             `(let ([,var* ,exp*] ...) ,exp)]
            [(letrec ([,label* (lambda ,uvar* ,[Expr -> body*])] ...) ,[Expr -> body])
             (begin
               (set! top-rec (append top-rec `([,label* (lambda ,uvar* ,body*)] ...)))
               body)]
            [(,prim ,[Expr -> exp*] ...) (guard (memq prim primitives))
                                          `(,prim ,exp* ...)]
            [(,[Expr -> exp] ,[Expr -> exp*] ...) `(,exp ,exp* ...)]
            [,x (error who "invalid Expr ~s" x)])))
        
        (let ([body (Expr prog)])
          `(letrec ,top-rec ,body))))

    (lambda (x)
      (Program x)))
  )

