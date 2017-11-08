(library 
 (Compiler expose-allocation-pointer)
 (export expose-allocation-pointer)
 (import (chezscheme) 
         (Framework helpers) 
         (Framework match))

 (define-who expose-allocation-pointer
   
   (define Body
     (lambda (x)
       (match x
         [(locals (,uvar* ...)
                  (new-frames (,nfv* ...) ,[Tail -> tail]))
          `(locals (,uvar* ...)
                   (new-frames (,nfv* ...) ,tail))]
         [,x (error who "invalid Body ~s" x)])))
   
   (define Tail
     (lambda (tail)
       (match tail
         [(begin ,[Effect -> ef*] ... ,[tail]) (make-begin `(,ef* ... ,tail))]
         [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
         [(,triv ,loc* ...) `(,triv ,loc* ...)]
         [,x (error who "invalid Tail ~s" x)])))
   
   (define Effect
     (lambda (ef)
       (match ef
         [(nop) '(nop)]
         [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
         [(begin ,[Effect -> ef*] ... ,[Effect -> ef]) (make-begin `(,ef* ... ,ef))]
         [(set! ,var (alloc ,triv))
          (make-begin `((set! ,var ,allocation-pointer-register)
                        (set! ,allocation-pointer-register
                              (+ ,allocation-pointer-register 
                                 ,triv))))]
         [(set! ,lhs ,rhs) `(set! ,lhs ,rhs)]
         [(mset! ,triv1 ,triv2 ,triv3) `(mset! ,triv1 ,triv2 ,triv3)]
         [(return-point ,label ,[Tail -> tail]) `(return-point ,label ,tail)]
         [,x (error who "invalid Effect list ~s" x)])))
   
   (define Pred
     (lambda (pred)
       (match pred
         [(true) '(true)]
         [(false) '(false)]
         [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
         [(begin ,[Effect -> ef*] ... ,[pred]) (make-begin `(,ef* ... ,pred))]
         [(,relop ,triv1 ,triv2) `(,relop ,triv1 ,triv2)]
         [,x (error who "invalid Pred ~s" x)])))
   
(lambda (x)
  (match x
    [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
     `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
    [,x (error who "invalid Program ~s" x)])))
)