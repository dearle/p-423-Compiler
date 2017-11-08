(library 
 (Compiler discard-call-live)
 (export discard-call-live)
 (import (chezscheme)
         (Framework helpers)
         (Framework match))
 
 (define-who discard-call-live
   
   (define Tail
     (lambda (tail)
       (match tail
         [(begin ,[Effect -> ef*] ... ,[tail]) `(begin ,ef* ... ,tail)]
         [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
         [(,t ,etc* ...) `(,t)]
         [,tail (error who "invalid Tail ~s" tail)])))
   
   (define Pred
     (lambda (pred)
       (match pred
         [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
         [(begin ,[Effect -> ef*] ... ,[pred]) `(begin ,ef* ... ,pred)]
         [,x x])))
   
   (define Effect
     (lambda (eff)
       (match eff
         [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
         [(begin ,[ef*] ... ,[eff]) `(begin ,ef* ... ,eff)]
         [(return-point ,rp-label ,[Tail -> tail]) `(return-point ,rp-label ,tail)]
         [,x x])))
   
   (define Body
     (lambda (bd)
       (match bd
         [(locate ([,uvar* ,loc*] ...) ,[Tail -> tail])
          `(locate ([,uvar* ,loc*] ...) ,tail)]
         [,bd (error who "invalid Body ~s" bd)])))
   
   (lambda (x)
     (match x
       [(letrec ([,label* (lambda () ,[Body -> bd*])] ...) ,[Body -> bd])
        `(letrec ([,label* (lambda () ,bd*)] ...) ,bd)]
       [,x (error who "invalid Program ~s" x)])))
 )