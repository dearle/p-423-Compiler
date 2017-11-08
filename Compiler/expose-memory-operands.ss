(library 
 (Compiler expose-memory-operands)
 (export expose-memory-operands)
 (import (chezscheme)
         (Framework helpers)
         (Framework match))
 
 (define-who expose-memory-operands
   
   (define Tail 
     (lambda (tail)
       (match tail
         [(if ,[Pred -> pred] ,[conseq] ,[altern]) `(if ,pred ,conseq ,altern)]
         [(begin ,[Effect -> effect*] ... ,[tail]) `(begin ,effect* ... ,tail)]
         [(,triv) `(,triv)]
         [,x (error who "invalid Tail ~s" x)])))
   
   (define Pred 
     (lambda (pred)
       (match pred
         [(true) '(true)]
         [(false) '(false)]
         [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
         [(begin ,[Effect -> effect*] ... ,[pred]) (make-begin `(,effect* ... ,pred))]
         [(,relop ,triv1 ,triv2) `(,relop ,triv1 ,triv2)]
         [,x (error who "invalid Pred ~s" x)])))

   (define Effect
       (lambda (ef)
         (match ef
           [(nop) '(nop)]
           [(begin ,[ef*] ... ,[ef]) (make-begin `(,ef* ... ,ef))]
           [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
           [(return-point ,label ,[Tail -> tail]) `(return-point ,label ,tail)]
           [(mset! ,base-expr ,offset-expr ,val) 
            `(set! ,(if (and (register? base-expr) 
                                  (register? offset-expr))
                             (make-index-opnd base-expr offset-expr)
                             (make-disp-opnd base-expr offset-expr)) ,val)]
           [(set! ,var (mref ,base-expr ,offset-expr))
            `(set! ,var ,(if (and (register? base-expr) 
                                  (register? offset-expr))
                             (make-index-opnd base-expr offset-expr)
                             (make-disp-opnd base-expr offset-expr)))]
           [(set! ,var (,binop ,t1 ,t2))
            `(set! ,var (,binop ,t1 ,t2))]
           [(set! ,var ,triv) `(set! ,var ,triv)]
           [,x (error who "invalid syntax for Effect ~s" x)])))
   
   (lambda (prog)
     (match prog
       [(letrec ([,label* (lambda () ,[Tail -> tail*])] ...) ,[Tail -> tail])
        `(letrec ([,label* (lambda () ,tail*)] ...) ,tail)]
       [,x (error who "invalid Program ~s" x)])))
 )