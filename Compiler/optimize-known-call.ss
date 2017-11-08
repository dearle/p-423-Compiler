(library 
 (Compiler optimize-known-call)
 (export optimize-known-call)
 (import
  (chezscheme)
  (Framework match)
  (Framework helpers))
 
 (define-who optimize-known-call
   
   (define primitives
     '(+ - * <= < = >= > boolean? car cdr cons eq? fixnum? make-vector null? pair? procedure?
         set-car! set-cdr! vector? vector-length vector-ref vector-set! void))

   (define Expr
     (lambda (known-procs)
       (lambda (x)
         (match x
           [,uvar (guard (uvar? uvar)) uvar]
           [,label (guard (label? label)) label]
           [(quote ,x) `(quote ,x)]
           [(if ,[(Expr known-procs) -> test] ,[(Expr known-procs) -> conseq] 
                ,[(Expr known-procs) -> altern]) 
            `(if ,test ,conseq ,altern)]
           [(begin ,[(Expr known-procs) -> ef*] ... ,[(Expr known-procs) -> ef]) 
            `(begin ,ef* ... ,ef)]
           [(let ([,uvar* ,[(Expr known-procs) -> exp*]] ...) ,[(Expr known-procs) -> exp]) 
            `(let ([,uvar* ,exp*] ...) ,exp)]
           [(letrec ([,label* (lambda (,uvar* ...)
                                (bind-free (,free-vars ...) ,body*))] ...) 
              (closures (,clos-bind* ...) ,body))
            (let* ([assoc-list (map (lambda (ls) 
                                      (cons (car ls) (cadr ls))) clos-bind*)]
                   [new-known-list (union assoc-list known-procs)]
                   [exp* (map (lambda (ls)
                                ((Expr new-known-list) ls)) body*)]
                   [exp ((Expr new-known-list) body)])
              `(letrec ([,label* (lambda (,uvar* ...) (bind-free (,free-vars ...) ,exp*))] ...) 
                 (closures (,clos-bind* ...) ,exp)))]
           [(,prim ,[(Expr known-procs) -> x*] ...) 
            (guard (memq prim primitives))
            `(,prim ,x* ...)]
           [(,uvar ,uvar ,[(Expr known-procs) -> rand] ...)
            (if (assq uvar known-procs)
                `(,(cdr (assq uvar known-procs)) ,uvar ,rand ...)
                `(,uvar ,uvar ,rand ...))]
           [,x (error who "invalid Expr ~s" x)]))))

   (lambda (x)
     ((Expr '()) x)))
 )