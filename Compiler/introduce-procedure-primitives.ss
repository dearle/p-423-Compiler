(library 
 (Compiler introduce-procedure-primitives)
 (export introduce-procedure-primitives)
 (import
  (chezscheme)
  (Framework match)
  (Framework helpers))
 
 (define-who introduce-procedure-primitives
   
   (define primitives
     '(+ - * <= < = >= > boolean? car cdr cons eq? fixnum? make-vector null? pair? procedure?
         set-car! set-cdr! vector? vector-length vector-ref vector-set! void))
   
   (define assoc-free-ls
     (lambda (ls num)
       (if (null? ls)
          '()
          (cons (cons (car ls) `(quote ,num)) (assoc-free-ls (cdr ls) (add1 num))))))
   
   (define Lambda
     (lambda (x)
       (match x
         [(lambda (,bound* ...) (bind-free (,clos-var ,free-ls ...) ,body))
          (let ([body ((Expr clos-var (assoc-free-ls free-ls 0)) body)])
            `(lambda (,bound* ...) ,body))]
         [,x (error who "invalid Lambda ~s" x)])))
   
   (define Closurize
     (lambda (clos-var free-ls)
       (lambda (closure)
         (let ([num -1])
           
           (define count-and-set
             (lambda (free-vars name)
               (if (null? free-vars)
                   '()
                   (begin
                     (set! num (add1 num))
                     (let ([a-set `(procedure-set! ,name (quote ,num) 
                                                   ,(if (assq (car free-vars) free-ls)
                                                        `(procedure-ref ,clos-var 
                                                                        ,(cdr (assq (car free-vars) free-ls)))
                                                        (car free-vars)))])
                       (cons a-set (count-and-set (cdr free-vars) name)))))))
           
           (match closure
             [(,name ,label) (values `(,name (make-procedure ,label '0)) '())]
             [(,name ,label ,free-vars ...)
              (let* ([clos-sets (count-and-set free-vars name)])
                (values `(,name (make-procedure ,label (quote ,(add1 num)))) `(,clos-sets ...)))] 
             [,x (error who "invalid closure ~s" x)])))))
     
   (define Expr
     (lambda (clos-var free-ls)
       (lambda (x)
         (match x
           [,uvar (guard (uvar? uvar)) (if (assq uvar free-ls)
                                           `(procedure-ref ,clos-var 
                                                           ,(cdr (assq uvar free-ls)))
                                           uvar)]
           [,label (guard (label? label)) label]
           [(quote ,x) `(quote ,x)]
           [(if ,[(Expr clos-var free-ls) -> test] 
                ,[(Expr clos-var free-ls) -> conseq] 
                ,[(Expr clos-var free-ls) -> altern]) `(if ,test ,conseq ,altern)]
           [(begin ,[(Expr clos-var free-ls) -> ef*] ... 
                   ,[(Expr clos-var free-ls) -> ef]) `(begin ,ef* ... ,ef)]
           [(let ([,uvar* ,[(Expr clos-var free-ls) -> exp*]] ...) 
              ,[(Expr clos-var free-ls) -> exp]) `(let ([,uvar* ,exp*] ...) ,exp)]
           [(letrec ([,label* ,[Lambda -> lambda*]] ...) 
              (closures (,[(Closurize clos-var free-ls) -> clos-make* clos-set*] ...) ,[(Expr clos-var free-ls) -> body])) 
            `(letrec ([,label* ,lambda*] ...) 
               (let (,clos-make* ...) 
                 ,(make-begin `(,clos-set* ... ... ,body))))]
           [(,prim ,[(Expr clos-var free-ls) -> x*] ...) (guard (memq prim primitives)) 
                                                         `(,prim ,x* ...)]
           [(,uvar ,uvar ,[(Expr clos-var free-ls) -> rand] ...)
            (guard (uvar? uvar)) 
            (if (assq uvar free-ls)
                `((procedure-code (procedure-ref ,clos-var 
                                                ,(cdr (assq uvar free-ls)))) 
                  (procedure-ref ,clos-var 
                                 ,(cdr (assq uvar free-ls))) ,rand ...)
                `((procedure-code ,uvar) ,uvar ,rand ...))]
           [(,label ,uvar ,[(Expr clos-var free-ls) -> rand] ...) 
            (if (assq uvar free-ls)
                `(,label (procedure-ref ,clos-var 
                                 ,(cdr (assq uvar free-ls))) ,rand ...)
                `(,label ,uvar ,rand ...))]
           [,x (error who "invalid Expr ~s" x)]))))

   (lambda (x)
     ((Expr '() '()) x)))
 )