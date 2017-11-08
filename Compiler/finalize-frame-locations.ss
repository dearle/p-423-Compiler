(library 
 (Compiler finalize-frame-locations)
 (export finalize-frame-locations)
 (import (chezscheme) 
         (Framework match) 
         (Framework helpers))

(define-who finalize-frame-locations
  
  (define Var
    (lambda (env)
      (lambda (v)
        (cond
          [(and (uvar? v) (assq v env)) => cdr]
          [else v]))))
  
  (define Triv Var)
  
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
           (if (eq? y x) `(nop) `(set! ,x ,y))]
          [(mset! ,[(Triv env) -> x] ,[(Triv env) -> y] ,[(Triv env) -> z])
           `(mset! ,x ,y ,z)]
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
          [(,[(Triv env) -> t] ,[(Triv env) -> live*] ...) `(,t ,live* ...)]
          [,tail (error who "invalid Tail ~s" tail)]))))
  
  (define Body
    (lambda (bd)
      (match bd
        [(locals (,local* ...)
           (ulocals (,ulocal* ...)
             (locate ([,uvar* ,loc*] ...)
               (frame-conflict ,ct ,[(Tail (map cons uvar* loc*)) -> tail]))))
         `(locals (,local* ...)
            (ulocals (,ulocal* ...)
              (locate ([,uvar* ,loc*] ...)
                (frame-conflict ,ct ,tail))))]
        [(locate ([,uvar* ,loc*] ...) ,tail)
         `(locate ([,uvar* ,loc*] ...) ,tail)]
        [,bd (error who "invalid Body ~s" bd)])))
  
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> bd*])] ...) ,[Body -> bd])
       `(letrec ([,label* (lambda () ,bd*)] ...) ,bd)]
      [,x (error who "invalid Program ~s" x)])))
)