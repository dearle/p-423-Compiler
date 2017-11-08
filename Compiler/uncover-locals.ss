(library 
 (Compiler uncover-locals)
 (export uncover-locals)
 (import
  (chezscheme)
  (Framework match)
  (Framework helpers))
 
 (define-who uncover-locals
   
   (define binops '(mref + - * logand logor sra))
   (define relops '(< > <= >= =))
   
   (define Body
     (lambda (body)
       (let ([tail (Tail body)])
         `(locals ,tail ,body))))
   
   (define Value*
     (lambda (val*)
       (match val*
         [() '()]
         [(,val* ... ,val) (append (Value* val*) (Value val))])))
   
   (define Value
     (lambda (val)
       (match val
         [(let ([,locals ,val*] ...) ,(Value -> locals**))
          (append locals (Value* val*) locals**)]
         [(if ,(Pred -> locals) ,(Value -> locals*) ,(Value -> locals**)) 
          (append locals locals* locals**)]
         [(begin ,eff* ... ,(Value -> locals))
          (append (Effect* eff*) locals)]
         [(,binop ,(Value -> locals) ,(Value -> locals*))
          (guard (memq binop binops))
          (append locals locals*)]
         [(alloc ,(Value -> locals)) locals]
         [(,(Value -> locals) ,val* ...)
          (append locals (Value* val*))]
         [,x '()])))
   
   (define Effect*
     (lambda (eff*)
       (match eff*
         [() '()]
         [(,effect* ... ,eff) (append (Effect* effect*) (Effect eff))])))
   
   (define Effect
     (lambda (ef)
       (match ef
         [(nop) '()]
         [(let ([,locals ,val*] ...) ,(Effect -> locals**))
          (append locals (Value* val*) locals**)]
         [(if ,(Pred -> locals) ,(Effect -> locals*) ,(Effect -> locals**)) 
          (append locals locals* locals**)]
         [(begin ,eff* ... ,(Effect -> locals))
          (append (Effect* eff*) locals)]
         [(mset! ,(Value -> locals)
                 ,(Value -> locals*) 
                 ,(Value -> locals**))
          (append locals locals* locals**)]
         [(,(Value -> locals) ,val* ...)
          (append locals (Value* val*))]
         [,ef (error who "invalid Effect ~s" ef)])))
   
   (define Pred
     (lambda (pr)
       (match pr
         [(true) '()]
         [(false) '()]
         [(let ([,locals ,val*] ...) ,(Pred -> locals**))
          (append locals (Value* val*) locals**)]
         [(if ,(Pred -> locals) ,(Pred -> locals*) ,(Pred -> locals**)) 
          (append locals locals* locals**)]
         [(begin ,eff* ... ,(Pred -> locals))
          (append (Effect* eff*) locals)]
         [(,relop ,(Value -> locals) ,(Value -> locals*))
          (guard (memq relop relops))
          (append locals locals*)]
         [,pr (error who "invalid Pred ~s" pr)])))
   
   (define Tail
     (lambda (tail)
       (match tail
         [(let ([,locals ,val*] ...) ,(Tail -> locals**))
          (append locals (Value* val*) locals**)]
         [(if ,(Pred -> locals) ,(Tail -> locals*) ,(Tail -> locals**)) 
          (append locals locals* locals**)]
         [(begin ,eff* ... ,(Tail -> locals))
          (append (Effect* eff*) locals)]
         [(,binop ,(Value -> locals) ,(Value -> locals*))
          (guard (memq binop binops))
          (append locals locals*)]
         [(alloc ,(Value -> locals)) locals]
         [(,(Value -> locals) ,val* ...)
          (append locals (Value* val*))]
         [,x '()])))
   
   (lambda (x)    
     (match x
       [(letrec ([,label* (lambda (,uvar* ...) 
                            ,(Body -> body*))] ...) ,(Body -> body))
        `(letrec ([,label* (lambda (,uvar* ...) 
                             ,body*)] ...) ,body)]
       [,x (error who "invalid Program ~s" x)]))))

