(library 
 (Compiler expose-frame-var)
 (export expose-frame-var)
 (import (chezscheme)
         (Framework helpers)
         (Framework match))
 
 (define-who expose-frame-var
   (lambda (program)
     
     (define offset 0)
     
     (define Triv
       (lambda (t)
         (if (frame-var? t)
             (make-disp-opnd frame-pointer-register
                             (- (ash (frame-var->index t) word-shift) offset))
             t)))
     
     (define Pred
       (lambda (pr)
         (match pr
           [(true) '(true)]
           [(false) '(false)]
           [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
           [(begin ,[Effect -> ef*] ... ,[pred])
            (make-begin `(,ef* ... ,pred))]
           [(,relop ,[Triv -> tr1] ,[Triv -> tr2]) `(,relop ,tr1 ,tr2)]
           [,pr (error who "invalid Pred ~s" pr)])))
     
     (define Effect
       (lambda (st)
         (match st
           [(nop) '(nop)]
           [(begin ,[ef*] ... ,[ef]) (make-begin `(,ef* ... ,ef))]
           [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
           [(return-point ,label ,[Tail -> tail]) `(return-point ,label ,tail)]
           [(mset! ,base-expr ,offset-expr ,val) `(mset! ,base-expr ,offset-expr ,val)]
           [(set! ,fp (- ,fp ,nb)) 
            (guard (eq? fp frame-pointer-register))
            (set! offset (+ offset nb))
            `(set! ,fp (- ,fp ,nb))]
           [(set! ,fp (+ ,fp ,nb))
            (guard (eq? fp frame-pointer-register))
            (set! offset (- offset nb))
            `(set! ,fp (+ ,fp ,nb))]
           [(set! ,[Triv -> var] (,binop ,[Triv -> t1] ,[Triv -> t2]))
            `(set! ,var (,binop ,t1 ,t2))]
           [(set! ,[Triv -> var] ,[Triv -> t]) `(set! ,var ,t)]
           [,st (error who "invalid syntax for Effect ~s" st)])))
     
     (define Tail
       (lambda (tail)
         (match tail
           [(,[Triv -> triv]) `(,triv)]
           [(begin ,[Effect -> ef*] ... ,[tail]) (make-begin `(,ef* ... ,tail))]
           [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
           [,tr (Triv tr)]
           [,tail (error who "invalid syntax for Tail ~s" tail)])))
     
     
     (match program
       [(letrec ([,label* (lambda (,uvar* ...) ,[Tail -> tail*])] ...) ,[Tail -> tail])
        `(letrec ([,label* (lambda (,uvar* ...) ,tail*)] ...) ,tail)]
       [,program (error who "invalid syntax for Program: ~s" program)])))
 
 )