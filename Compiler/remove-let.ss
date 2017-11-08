(library 
 (Compiler remove-let)
 (export remove-let)
 (import
  (chezscheme)
  (Framework match)
  (Framework helpers))
 
 (define-who remove-let
   
   (define binops '(mref + - * logand logor sra))
   (define relops '(< > <= >= =))
   
   (define Body
     (lambda (body)
       (match body
         [(locals ,uvar* ,(Tail -> tail))
           `(locals ,uvar* ,tail)]
         [,x (error who "invalid Body ~s" x)])))
   
   (define Value
     (lambda (val)
       (match val
         [(let ([,set-var* ,(Value -> val*)] ...) ,(Value -> val))
          (make-begin `((set! ,set-var* ,val*) ... ,val))]
         [(if ,(Pred -> test) ,(Value -> conseq) ,(Value -> altern)) 
          `(if ,test ,conseq ,altern)]
         [(begin ,(Effect -> eff*) ... ,(Value -> val))
          (make-begin `(,eff* ... ,val))]
         [(,binop ,(Value -> val1) ,(Value -> val2))
          (guard (memq binop binops))
          `(,binop ,val1 ,val2)]
         [(alloc ,(Value -> val)) 
          `(alloc ,val)]
         [(,(Value -> val) ,(Value -> val*) ...)
          `(,val ,val* ...)]
         [,x x])))
   
   (define Effect
     (lambda (ef)
       (match ef
         [(nop) '(nop)]
         [(let ([,set-var* ,(Value -> values*)] ...) ,(Effect -> effect))
          (make-begin `((set! ,set-var* ,values*) ... ,effect))]
         [(if ,(Pred -> test) ,(Effect -> conseq) ,(Effect -> altern)) 
          `(if ,test ,conseq ,altern)]
         [(begin ,(Effect -> eff*) ... ,(Effect -> eff))
          (make-begin `(,eff* ... ,eff))]
         [(mset! ,(Value -> val1)
                 ,(Value -> val2) 
                 ,(Value -> val3))
          `(mset! ,val1 ,val2 ,val3)]
         [(,(Value -> val) ,(Value -> val*) ...)
          `(,val ,val* ...)]
         [,ef (error who "invalid Effect ~s" ef)])))
   
   (define Pred
     (lambda (pr)
       (match pr
         [(true) '(true)]
         [(false) '(false)]
         [(let ([,set-var* ,(Value -> values*)] ...) ,(Pred -> pred))
          (make-begin `((set! ,set-var* ,values*) ... ,pred))]
         [(if ,(Pred -> test) ,(Pred -> conseq) ,(Pred -> altern)) 
          `(if ,test ,conseq ,altern)]
         [(begin ,(Effect -> effect*) ... ,(Pred -> pred))
          (make-begin `(,effect* ... ,pred))]
         [(,relop ,(Value -> val1) ,(Value -> val2))
          (guard (memq relop relops))
          `(,relop ,val1 ,val2)]
         [,pr (error who "invalid Pred ~s" pr)])))
   
   (define Tail
     (lambda (tail)
       (match tail
         [(let ([,set-var* ,(Value -> values*)] ...) ,(Tail -> new-tail))
          (make-begin `((set! ,set-var* ,values*) ... ,new-tail))]
         [(if ,(Pred -> test) ,(Tail -> conseq) ,(Tail -> altern)) 
          `(if ,test ,conseq ,altern)]
         [(begin ,(Effect -> effect*) ... ,(Tail -> new-tail))
          (make-begin `(,effect* ... ,new-tail))]
         [(,binop ,(Value -> val1) ,(Value -> val2))
          (guard (memq binop binops))
          `(,binop ,val1 ,val2)]
         [(alloc ,(Value -> val)) 
          `(alloc ,val)]
         [(,(Value -> val) ,(Value -> val*) ...)
          `(,val ,val* ...)]
         [,x x])))
   
   (lambda (x)    
     (match x
       [(letrec ([,label* (lambda (,uvar* ...) 
                            ,(Body -> body*))] ...) ,(Body -> body))
        `(letrec ([,label* (lambda (,uvar* ...) 
                             ,body*)] ...) ,body)]
       [,x (error who "invalid Program ~s" x)]))))

