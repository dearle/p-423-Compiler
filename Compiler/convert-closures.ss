(library 
 (Compiler convert-closures)
 (export convert-closures)
 (import
  (chezscheme)
  (Framework match)
  (Framework helpers))
 
 (define-who convert-closures
   
   (define primitives
     '(+ - * <= < = >= > boolean? car cdr cons eq? fixnum? make-vector null? pair? procedure?
         set-car! set-cdr! vector? vector-length vector-ref vector-set! void))
   
   (define Letrec
     (lambda (x)
       (match x
         [(,name-var (lambda (,bound* ...) (free ,free* ,[Expr -> body])))
          (let ([cp (unique-name 'cp)]
                [label (unique-label name-var)])
           (values `[,label (lambda (,cp ,bound* ...) (bind-free (,cp ,free* ...) ,body))] 
                   `[,name-var ,label ,free* ...]))]      
         [,x (error who "invalid Lambda ~s" x)])))
   
   (define Expr
     (lambda (x)
       (match x
         [,uvar (guard (uvar? uvar)) uvar]
         [(quote ,x) `(quote ,x)]
         [(if ,[Expr -> test] ,[Expr -> conseq] ,[Expr -> altern]) `(if ,test ,conseq ,altern)]
         [(begin ,[Expr -> ef*] ... ,[Expr -> ef]) `(begin ,ef* ... ,ef)]
         [(let ([,uvar* ,[Expr -> exp*]] ...) ,[Expr -> exp]) 
          `(let ([,uvar* ,exp*] ...) ,exp)]
         [(letrec (,[Letrec -> lambda* closures*] ...) ,[Expr -> x]) 
          `(letrec (,lambda* ...) (closures ,closures* ,x))]
         [(,prim ,[Expr -> x*] ...) (guard (memq prim primitives)) `(,prim ,x* ...)]
         [(,uvar ,[Expr -> rand] ...) (guard (uvar? uvar)) `(,uvar ,uvar ,rand ...)]
         [(,[Expr -> rator] ,[Expr -> rand] ...) (let ([tmp (unique-name 'tmp)])
                                                   `(let ([,tmp ,rator])
                                                      (,tmp ,tmp ,rand ...)))]
         [,x (error who "invalid Expr ~s" x)])))
   
   (lambda (x)
     (Expr x)))
 )

