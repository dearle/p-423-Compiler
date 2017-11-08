(library 
 (Compiler convert-assignments)
 (export convert-assignments)
 (import (chezscheme)
         (Framework helpers)
         (Framework match))
 
 (define-who convert-assignments
   
   (define primitives
     '(+ - * <= < = >= > boolean? car cdr cons eq? fixnum? make-vector null? pair? procedure?
         set-car! set-cdr! vector? vector-length vector-ref vector-set! void))
   
   (define Expr
     (lambda (assigns)
       (lambda (exp)
         (match exp
           [,u (guard (uvar? u)) (if (memq u assigns)
                                     `(car ,u)
                                     u)]
           [(quote ,x) `(quote ,x)]
           [(if ,[(Expr assigns) -> test] ,[(Expr assigns) -> conseq] 
                ,[(Expr assigns) -> altern]) `(if ,test ,conseq ,altern)]
           [(begin ,[(Expr assigns) -> eff*] ... ,[(Expr assigns) -> eff])
            (make-begin `(,eff* ... ,eff))]
           [(lambda (,uvar* ...) (assigned (,a* ...) ,body))
            (let* ([assocs '()]
                   [new-ass (union a* assigns)]
                   [new-body ((Expr new-ass) body)]
                   [new-let-binds (map (lambda (var)
                                          (let ([new-t (unique-name var)])
                                            (set! assocs (cons (cons var new-t) assocs))
                                            `(,var (cons ,new-t (void))))) a*)]
                   [new-vars (map (lambda (var)
                                     (if (assq var assocs)
                                         (cdr (assq var assocs))
                                         var)) uvar*)])
                  (if (null? new-let-binds)
                      `(lambda (,new-vars ...) ,new-body)
                      `(lambda (,new-vars ...) (let (,new-let-binds ...) ,new-body))))]
           [(let (,binds ...) (assigned (,a* ...) ,body))
            (let* ([assocs '()]
                   [new-ass (union a* assigns)]
                   [new-body ((Expr new-ass) body)]
                   [new-let-binds (map (lambda (var)
                                          (let ([new-t (unique-name var)])
                                            (set! assocs (cons (cons var new-t) assocs))
                                            `(,var (cons ,new-t (void))))) a*)]
                   [new-binds (map (lambda (b)
                                     (if (assq (car b) assocs)
                                         `(,(cdr (assq (car b) assocs)) ,((Expr new-ass) (cadr b)))
                                         `(,(car b) ,((Expr new-ass) (cadr b))))) binds)])
              (if (null? new-binds)
                  (if (null? new-let-binds)
                      new-body
                      `(let (,new-let-binds ...) ,new-body))
                  (if (null? new-let-binds)
                      `(let (,new-binds ...) ,new-body)
                      `(let (,new-binds ...) (let (,new-let-binds ...) ,new-body)))))]
           [(letrec ([,uvar* ,[(Expr assigns) -> exp*]] ...) ,[(Expr assigns) -> body])
            `(letrec ([,uvar* ,exp*] ...) ,body)]
           [(set! ,uvar ,[(Expr assigns) -> exp]) `(set-car! ,uvar ,exp)]
           [(,prim ,[(Expr assigns) -> exp*] ...) 
            (guard (memq prim primitives)) 
            `(,prim ,exp* ...)]
           [(,[(Expr assigns) -> rator] ,[(Expr assigns) -> rand] ...)
            `(,rator ,rand ...)]
           [,x (error who "invalid Expr ~s" x)]))))
   
   (lambda (exp)
     ((Expr '()) exp)))
 )