(library 
 (Compiler remove-complex-opera*)
 (export remove-complex-opera*)
 (import (chezscheme) 
         (Framework helpers) 
         (Framework match))
 
 (define-who remove-complex-opera*
   
   (define Body
     (lambda (bd)
       
       (define new-local* '())
       
       (define new-t
         (lambda ()
           (let ([t (unique-name 't)])
             (set! new-local* (cons t new-local*))
             t)))
     
       (define trivialize-call 
         (lambda (expr*)
           (let-values ([(call set*) (break-down-expr* expr*)])
             (make-begin `(,@set* ,call)))))
     
       (define break-down-expr* 
         (lambda (expr*)
           (match expr*
             [() (values '() '())]
             [(,s . ,[rest* set*])
              (guard (simple? s))
              (values `(,s ,rest* ...) set*)]
             [(,[Value -> expr] . ,[rest* set*])
              (let ([t (new-t)])
                (values `(,t ,rest* ...) `((set! ,t ,expr) ,set* ...)))]
             [,expr* (error who "invalid Expr ~s" expr*)])))
     
       (define simple? 
         (lambda (x)
           (or (uvar? x) (label? x) (and (integer? x) (exact? x))
               (memq x '(+ - * logand logor sra)) (memq x '(= < <= > >=))
               (memq x '(alloc mref mset!)))))
       
       (define triv? 
         (lambda (x) (or (uvar? x) (int64? x) (label? x))))
     
       (define Value 
         (lambda (val)
           (match val
             [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
             [(begin ,[Effect -> ef*] ... ,[val]) (make-begin `(,ef* ... ,val))]
             [(,binop ,x ,y)
              (guard (memq binop '(+ - * logand logor sra)))
              (trivialize-call `(,binop ,x ,y))]
             [(alloc ,val) (trivialize-call `(alloc ,val))]
             [(mref ,val1 ,val2) (trivialize-call `(mref ,val1 ,val2))]
             [(,rator ,rand* ...) (trivialize-call `(,rator ,rand* ...))]
             [,tr (guard (triv? tr)) tr]
             [,val (error who "invalid Value ~s" val)])))
       
     (define Effect 
       (lambda (ef)
         (match ef
           [(nop) '(nop)]
           [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
           [(begin ,[ef*] ... ,[ef]) (make-begin `(,ef* ... ,ef))]
           [(set! ,var ,[Value -> val]) `(set! ,var ,val)]
           [(mset! ,val1 ,val2 ,val3) (trivialize-call `(mset! ,val1 ,val2 ,val3))]
           [(,rator ,rand* ...) (trivialize-call `(,rator ,rand* ...))]
           [,ef (error who "invalid Effect ~s" ef)])))
       
     (define Pred 
       (lambda (pr)
         (match pr
           [(true) '(true)]
           [(false) '(false)]
           [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
           [(begin ,[Effect -> ef*] ... ,[pr]) (make-begin `(,ef* ... ,pr))]
           [(,relop ,x ,y)
            (guard (memq relop '(< <= = >= >)))
            (trivialize-call `(,relop ,x ,y))]
           [,pr (error who "invalid Pred ~s" pr)])))
       
     (define Tail 
       (lambda (tail)
         (match tail
           [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
           [(begin ,[Effect -> ef*] ... ,[tail]) (make-begin `(,ef* ... ,tail))]
           [(,binop ,x ,y)
            (guard (memq binop '(+ - * logand logor sra)))
            (trivialize-call `(,binop ,x ,y))]
           [(alloc ,val) (trivialize-call `(alloc ,val))]
           [(mref ,val1 ,val2) (trivialize-call `(mref ,val1 ,val2))]
           [(,rator ,rand* ...) (trivialize-call `(,rator ,rand* ...))]
           [,tr (guard (triv? tr)) tr]
           [,tail (error who "invalid Tail ~s" tail)])))
       
       (match bd
         [(locals (,local* ...) ,[Tail -> tail])
          `(locals (,local* ... ,new-local* ...) ,tail)]
         [,bd (error who "invalid Body ~s" bd)])))
   
   (lambda (x)
     (match x
       [(letrec ([,label* (lambda (,fml** ...) ,[Body -> bd*])] ...)
          ,[Body -> bd])
        `(letrec ([,label* (lambda (,fml** ...) ,bd*)] ...) ,bd)]
       [,x (error who "invalid Program ~s" x)])))
 )