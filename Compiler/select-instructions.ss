(library 
 (Compiler select-instructions)
 (export select-instructions)
 (import (chezscheme) 
         (Framework match) 
         (Framework helpers))
 
 (define-who select-instructions
   
   (define ur?
     (lambda (x) 
       (or (register? x) (uvar? x))))
   
   (define Body 
     (lambda (x)
       
       (define new-ulocal* '())
       
       (define new-u
         (lambda ()
           (let ([u (unique-name 'u)])
             (set! new-ulocal* (cons u new-ulocal*))
             u)))
       
       (define select-binop-1
         (lambda (x op y z)
           (cond
             [(eq? y x) (select-binop-2 x op z)]
             [(and (eq? z x) (memq op '(* + logor logand))) (select-binop-2 x op y)]
             [else (let ([u (new-u)])
                     (make-begin `((set! ,u ,y)
                                   ,(select-binop-2 u op z)
                                   (set! ,x ,u))))])))
       
       (define select-binop-2 
         (lambda (x op y)
           (case op
             [(- + logand logor)
              (if (or (and (ur? x) (or (ur? y) (frame-var? y) (int32? y)))
                      (and (frame-var? x) (or (ur? y) (int32? y))))
                  `(set! ,x (,op ,x ,y))
                  (let ([u (new-u)])
                    (make-begin `((set! ,u ,y) (set! ,x (,op ,x ,u))))))]
             [(*)
              (if (ur? x)
                  (if (or (ur? y) (frame-var? y) (int32? y))
                      `(set! ,x (,op ,x ,y))
                      (let ([u (new-u)])
                        (make-begin `((set! ,u ,y) (set! ,x (,op ,x ,u))))))
                  (let ([u (new-u)])
                    (make-begin
                     `((set! ,u ,x)
                       ,(select-binop-2 u op y)
                       (set! ,x ,u)))))]
             [(sra) `(set! ,x (,op ,x ,y))]
             [else (error who "unrecognized binop ~s" op)])))
       
       (define select-move 
         (lambda (lhs rhs)
           (if (or (ur? lhs) (and (frame-var? lhs) (or (ur? rhs) (int32? rhs))))
               `(set! ,lhs ,rhs)
               (let ([u (new-u)])
                 (make-begin `((set! ,u ,rhs) (set! ,lhs ,u)))))))
       
       (define select-mset!-1 
         (lambda (base-expr offset-expr val)
           (if (ur? base-expr) 
                 (select-mset!-2 base-expr offset-expr val)
                 (let ([u (new-u)])
                   (make-begin `((set! ,u ,base-expr)
                                 ,(select-mset!-2 u offset-expr val)))))))
                                      
       (define select-mset!-2 
         (lambda (base-expr offset-expr val)
           (if (or (int32? offset-expr) (ur? offset-expr))
                 (select-mset!-3 base-expr offset-expr val)
                 (let ([u (new-u)])
                   (make-begin `((set! ,u ,offset-expr)
                                 ,(select-mset!-3 base-expr u val)))))))
       
       (define select-mset!-3 
         (lambda (base-expr offset-expr val)
           (if (or (int32? val) (ur? val))
               `(mset! ,base-expr ,offset-expr ,val)
               (let ([u (new-u)])
                 (make-begin `((set! ,u ,val)
                               (mset! ,base-expr ,offset-expr ,u)))))))
       
       (define select-mref-1 
         (lambda (var base-expr offset-expr)          
           (if (ur? var)
               (select-mref-2 var base-expr offset-expr)
               (let ([u (new-u)])
                 (make-begin `(,(select-mref-2 u base-expr offset-expr)
                               (set! ,var ,u)))))))
       
       (define select-mref-2
         (lambda (var base-expr offset-expr)          
           (if (ur? base-expr)
               (select-mref-3 var base-expr offset-expr)
               (let ([u (new-u)])
                 (make-begin `((set! ,u ,base-expr)
                               ,(select-mref-3 var u offset-expr)))))))
       
       (define select-mref-3
         (lambda (var base-expr offset-expr)          
           (if (or (ur? offset-expr) (int32? offset-expr))
               `(set! ,var (mref ,base-expr ,offset-expr))
               (let ([u (new-u)])
                 (make-begin `((set! ,u ,offset-expr)
                               (set! ,var (mref ,base-expr ,u))))))))
       
       (define select-relop-1 
         (lambda (op x y)
           
           (define swap-relop 
             (lambda (op)
               (cdr (assq op '((= . =) (< . >) (<= . >=) (> . <) (>= . <=))))))
           
           (cond
             [(or (ur? x) (frame-var? x)) (select-relop-2 op x y)]
             [(or (ur? y) (frame-var? y)) (select-relop-2 (swap-relop op) y x)]
             [else (let ([u (new-u)])
                     (make-begin `((set! ,u ,x)
                                   ,(select-relop-2 op u y))))])))
       
       (define select-relop-2 
         (lambda (op x y)
           (if (if (ur? x)
                   (or (ur? y) (frame-var? y) (int32? y))
                   (or (ur? y) (int32? y)))
               `(,op ,x ,y)
               (let ([u (new-u)])
                 (make-begin `((set! ,u ,y) (,op ,x ,u)))))))
       
       (define Effect 
         (lambda (x)
           (match x
             [(nop) '(nop)]
             [(begin ,[ef*] ... ,[ef]) (make-begin `(,ef* ... ,ef))]
             [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
             [(set! ,lhs (mref ,base-expr ,offset-expr)) (select-mref-1 lhs base-expr offset-expr)]
             [(set! ,lhs (,binop ,x ,y)) (select-binop-1 lhs binop x y)]
             [(set! ,lhs ,rhs) (select-move lhs rhs)]
             [(mset! ,base-expr ,offset-expr ,expr) (select-mset!-1 base-expr offset-expr expr)]
             [(return-point ,rp-label ,[Tail -> tail]) `(return-point ,rp-label ,tail)]
             [,x (error who "invalid Effect ~s" x)])))
       
       (define Pred 
         (lambda (x)
           (match x
             [(true) '(true)]
             [(false) '(false)]
             [(begin ,[Effect -> ef*] ... ,[test]) (make-begin `(,ef* ... ,test))]
             [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
             [(,relop ,x ,y) (select-relop-1 relop x y)]
             [,x (error who "invalid Pred ~s" x)])))
       
       (define Tail 
         (lambda (x)
           (match x
             [(begin ,[Effect -> ef*] ... ,[tail]) (make-begin `(,ef* ... ,tail))]
             [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
             [(,triv ,live* ...) `(,triv ,live* ...)]
             [,x (error who "invalid Tail ~s" x)])))
       
       (match x
         [(locals (,local* ...)
                  (ulocals (,ulocal* ...)
                           (locate (,home* ...) (frame-conflict ,ct ,[Tail -> tail]))))
          `(locals (,local* ...)
                   (ulocals (,ulocal* ... ,new-ulocal* ...)
                            (locate (,home* ...)
                                    (frame-conflict ,ct ,tail))))]
         [(locate (,home* ...) ,tail) `(locate (,home* ...) ,tail)]
         [,x (error who "invalid Body ~s" x)])))
   
   (lambda (x)
     (match x
       [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
        `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
       [,x (error who "invalid Program ~s" x)])))
 
 )
