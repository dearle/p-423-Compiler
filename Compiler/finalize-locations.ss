(library 
 (Compiler finalize-locations)
 (export finalize-locations)
 (import (chezscheme)
         (Framework helpers)
         (Framework match))
 
 (define-who finalize-locations
   
   (define Var
     (lambda (env)
       (lambda (v)
         (if (uvar? v) (cdr (assq v env)) v))))
   
   (define Triv
     (lambda (env)
       (lambda (t)
         (if (uvar? t) (cdr (assq t env)) t))))
   
   (define Pred
     (lambda (env)
       (lambda (pr)
         (match pr
           [(true) '(true)]
           [(false) '(false)]
           [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
           [(begin ,[(Effect env) -> ef*] ... ,[pr]) `(begin ,ef* ... ,pr)]
           [(,relop ,[(Triv env) -> x] ,[(Triv env) -> y]) `(,relop ,x ,y)]
           [,pr (error who "invalid Pred ~s" pr)]))))
   
   (define Effect
     (lambda (env)
       (lambda (ef)
         (match ef
           [(nop) '(nop)]
           [(set! ,[(Var env) -> x]
                  (,binop ,[(Triv env) -> y] ,[(Triv env) -> z]))
            `(set! ,x (,binop ,y ,z))]
           [(set! ,[(Var env) -> x] ,[(Triv env) -> y])
            (if (eq? x y) '(nop) `(set! ,x ,y))]
           [(mset! ,[(Triv env) -> base-expr] 
                   ,[(Triv env) -> offset-expr] 
                   ,[(Triv env) -> val])
            `(mset! ,base-expr ,offset-expr ,val)]
           [(begin ,[ef] ,[ef*] ...) `(begin ,ef ,ef* ...)]
           [(if ,[(Pred env) -> test] ,[conseq] ,[altern])
            `(if ,test ,conseq ,altern)]
           [(return-point ,rp-label ,[(Tail env) -> tail]) `(return-point ,rp-label ,tail)]
           [,ef (error who "invalid Effect ~s" ef)]))))
   
   (define Tail
     (lambda (env)
       (lambda (tail)
         (match tail
           [(begin ,[(Effect env) -> ef*] ... ,[tail]) `(begin ,ef* ... ,tail)]
           [(if ,[(Pred env) -> test] ,[conseq] ,[altern])
            `(if ,test ,conseq ,altern)]
           [(,[(Triv env) -> t]) `(,t)]
           [,tail (error who "invalid Tail ~s" tail)]))))
   
   (define Body
     (lambda (bd)
       (match bd
         [(locate ([,uvar* ,loc*] ...) ,[(Tail (map cons uvar* loc*)) -> tail])
          tail]
         [,bd (error who "invalid Body ~s" bd)])))
   
   (lambda (x)
     (match x
       [(letrec ([,label* (lambda () ,[Body -> bd*])] ...) ,[Body -> bd])
        `(letrec ([,label* (lambda () ,bd*)] ...) ,bd)]
       [,x (error who "invalid Program ~s" x)])))
 )