(library 
 (Compiler flatten-set!)
 (export flatten-set!)
 (import (chezscheme) 
         (Framework helpers) 
         (Framework match))
 
 (define-who flatten-set!
   
   (define trivialize-set! 
     (lambda (lhs rhs)
       (match rhs
         [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
         [(begin ,[Effect -> ef*] ... ,[rhs]) (make-begin `(,ef* ... ,rhs))]
         [(,binop ,[Triv -> x] ,[Triv -> y])
          (guard (memq binop '(+ - * logand logor sra)))
          `(set! ,lhs (,binop ,x ,y))]
         [(alloc ,[Triv -> triv]) `(set! ,lhs (alloc ,triv))]
         [(mref ,[Triv -> triv1] ,[Triv -> triv2]) `(set! ,lhs (mref ,triv1 ,triv2))]
         [(mset! ,[Triv -> triv1] ,[Triv -> triv2] ,[Triv -> triv3]) 
          `(set! ,lhs (mset! ,triv1 ,triv2 ,triv3))]
         [(,[Triv -> rator] ,[Triv -> rand*] ...) `(set! ,lhs (,rator ,rand* ...))]
         [,tr (guard (triv? tr)) `(set! ,lhs ,tr)]
         [,rhs (error who "invalid set! Rhs ~s" rhs)])))
   
   (define triv? 
     (lambda (x) 
       (or (uvar? x) (int64? x) (label? x))))
   
   (define Triv 
     (lambda (t) 
       (if (triv? t) t (error who "invalid Triv ~s" t))))
   
   (define Effect 
     (lambda (ef)
       (match ef
         [(nop) '(nop)]
         [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
         [(begin ,[Effect -> ef*] ... ,[ef]) (make-begin `(,ef* ... ,ef))]
         [(set! ,var ,val) (trivialize-set! var val)]
         [(mset! ,[Triv -> triv1] ,[Triv -> triv2] ,[Triv -> triv3]) `(mset! ,triv1 ,triv2 ,triv3)]
         [(,[Triv -> rator] ,[Triv -> rand*] ...) `(,rator ,rand* ...)]
         [,ef (error who "invalid Effect ~s" ef)])))
     
     (define Pred 
       (lambda (pr)
         (match pr
           [(true) '(true)]
           [(false) '(false)]
           [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
           [(begin ,[Effect -> ef*] ... ,[pr]) (make-begin `(,ef* ... ,pr))]
           [(,relop ,[Triv -> x] ,[Triv -> y])
            (guard (memq relop '(< <= = >= >)))
            `(,relop ,x ,y)]
           [,pr (error who "invalid Pred ~s" pr)])))
     
     (define Tail 
       (lambda (tail)
         (match tail
           [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
           [(begin ,[Effect -> ef*] ... ,[tail]) (make-begin `(,ef* ... ,tail))]
           [(,binop ,[Triv -> x] ,[Triv -> y])
            (guard (memq binop '(+ - * logand logor sra)))
            `(,binop ,x ,y)]
           [(alloc ,[Triv -> triv]) `(alloc ,triv)]
           [(mref ,[Triv -> triv1] ,[Triv -> triv2]) `(mref ,triv1 ,triv2)]
           [(,[Triv -> rator] ,[Triv -> rand*] ...) `(,rator ,rand* ...)]
           [,tr (guard (triv? tr)) tr]
           [,tail (error who "invalid Tail ~s" tail)])))
     
     (define Body 
       (lambda (bd)
         (match bd
           [(locals (,uvar* ...) ,[Tail -> tail]) `(locals (,uvar* ...) ,tail)]
           [,bd (error who "invalid Body ~s" bd)])))
     
     (lambda (x)
       (match x
         [(letrec ([,label* (lambda (,fml** ...) ,[Body -> bd*])] ...)
            ,[Body -> bd])
          `(letrec ([,label* (lambda (,fml** ...) ,bd*)] ...) ,bd)]
         [,x (error who "invalid Program ~s" x)])))
   
   )
 