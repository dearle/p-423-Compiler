(library 
 (Compiler uncover-assigned)
 (export uncover-assigned)
 (import
  (chezscheme)
  (Framework match)
  (Framework helpers))
 
 (define-who uncover-assigned
   
   (define primitives
     '(+ - * <= < = >= > boolean? car cdr cons eq? fixnum? make-vector null? pair? procedure?
         set-car! set-cdr! vector? vector-length vector-ref vector-set! void))
   
   (define Expr
     (lambda (x)
       (match x
         [,uvar (guard (uvar? uvar)) (values uvar '())]
         [(quote ,x) (values `(quote ,x) '())]
         [(if ,[Expr -> test-expr test-ass],[Expr -> conseq-expr conseq-ass] 
              ,[Expr -> altern-expr altern-ass])
          (values `(if ,test-expr ,conseq-expr ,altern-expr) 
                  (union test-ass (union conseq-ass altern-ass)))]
         [(begin ,[Expr -> eff*-expr eff*-ass] ... ,[Expr -> eff-expr eff-ass])
          (values `(begin ,eff*-expr ... ,eff-expr) (union (apply union eff*-ass) eff-ass))]
         [(lambda (,uvar* ...) ,[Expr -> exp-expr exp-ass])
          (let ([intersect (intersection uvar* exp-ass)])
            (values `(lambda (,uvar* ...) (assigned ,intersect ,exp-expr)) (difference exp-ass intersect)))]
         [(let ([,uvar* ,[Expr -> exp*-expr exp*-ass]] ...) ,[Expr -> exp-expr exp-ass])
          (let* ([assign-exp (union (apply union exp*-ass) exp-ass)]
                 [intersect (intersection uvar* assign-exp)])
            (values `(let ([,uvar* ,exp*-expr] ...) (assigned ,intersect ,exp-expr)) 
                    (difference assign-exp intersect)))]
         [(letrec ([,uvar* ,[Expr -> exp*-expr exp*-ass]] ...) ,[Expr -> exp-expr exp-ass])
          (let* ([assign-exp (union (apply union exp*-ass) exp-ass)]
                 [intersect (intersection uvar* assign-exp)])
            (values `(letrec ([,uvar* ,exp*-expr] ...) (assigned ,intersect ,exp-expr))
                    (difference assign-exp intersect)))]
         [(set! ,uvar ,[Expr -> exp-expr exp-ass])
          (values `(set! ,uvar ,exp-expr) (union exp-ass `(,uvar)))]  
         [(,prim ,[Expr -> exp-expr exp-ass] ...) 
          (guard (memq prim primitives)) 
          (values `(,prim ,exp-expr ...) (apply union exp-ass))]
         [(,[Expr -> rator-expr rator-ass] ,[Expr -> rand*-expr rand*-ass] ...)
          (values `(,rator-expr ,rand*-expr ...) (union rator-ass (apply union rand*-ass)))]
         [,x (error who "invalid Expr ~s" x)])))
   
   (lambda (x) 
     (let-values ([(expr assigned) (Expr x)])
       expr)))
 )