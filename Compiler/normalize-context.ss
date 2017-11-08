(library (Compiler normalize-context)
  (export normalize-context)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers))

  (define-who normalize-context
    
    (define value-prim
      '(+ - * car cdr cons make-vector vector-length vector-ref make-procedure
          procedure-code procedure-ref void))
    
    (define pred-prim
      '(<= < = >= > boolean? eq? fixnum? null? pair? vector? procedure?))
    
    (define effect-prim
      '(set-car! set-cdr! vector-set! procedure-set!))
    
    (define (make-nopless-begin x*)
      (let ([x* (remove '(nop) x*)])
        (if (null? x*)
            '(nop)
            (make-begin x*))))
      
    (define Value
      (lambda (x)
        (match x
          [,label (guard (label? label)) label]
          [,uvar (guard (uvar? uvar)) uvar]
          [(quote ,x) `(quote ,x)]
          [(if ,[Pred -> test] ,[Value -> conseq] ,[Value -> altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> ef*] ... ,[Value -> val]) (make-begin `(,ef* ... ,val))]
          [(let ([,var* ,[Value -> val*]] ...) ,[Value -> val])
           `(let ([,var* ,val*] ...) ,val)]
          [(,prim ,[Value -> exp*] ...) (guard (memq prim pred-prim))
                                        `(if (,prim ,exp* ...) '#t '#f)]
          [(,prim ,[Value -> exp*] ...) (guard (memq prim effect-prim)) 
                                        `(begin (,prim ,exp* ...) (void))]
          [(,prim ,[Value -> exp*] ...) (guard (memq prim value-prim)) 
                                        `(,prim ,exp* ...)]
          [(,[Value -> exp] ,[Value -> exp*] ...) `(,exp ,exp* ...)]
          [,x (error who "invalid Val ~s" x)])))
    
    (define Pred
      (lambda (x)
        (match x
          [,label (guard (label? label)) label]
          [,uvar (guard (uvar? uvar)) `(if (eq? ,uvar '#f) (false) (true))]
          [(quote ,x) (if (eq? x #f)
                          '(false)
                          '(true))]
          [(if ,[Pred -> test] ,[Pred -> conseq] ,[Pred -> altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> ef*] ... ,[Pred -> pred]) (make-begin `(,ef* ... ,pred))]
          [(let ([,var* ,[Value -> val*]] ...) ,[Pred -> pred])
           `(let ([,var* ,val*] ...) ,pred)]
          [(,prim ,[Value -> exp*] ...) (guard (memq prim pred-prim))
                                        `(,prim ,exp* ...)]
          [(,prim ,[Value -> exp*] ...) (guard (memq prim effect-prim)) 
                                        `(begin (,prim ,exp* ...) (true))]
          [(,prim ,[Value -> exp*] ...) (guard (memq prim value-prim)) 
                                        `(if (eq? (,prim ,exp* ...) '#f) (false) (true))]
          [(,[Value -> exp] ,[Value -> exp*] ...) 
           `(if (eq? (,exp ,exp* ...) '#f) (false) (true))]
          [,x (error who "invalid Pred ~s" x)])))
    
    (define Effect
      (lambda (x)
        (match x
          [,label (guard (label? label)) '(nop)]
          [,uvar (guard (uvar? uvar)) '(nop)]
          [(quote ,x) '(nop)]
          [(if ,[Pred -> test] ,[Effect -> conseq] ,[Effect -> altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> ef*] ... ,[Effect -> ef]) (make-begin `(,ef* ... ,ef))]
          [(let ([,var* ,[Value -> val*]] ...) ,[Effect -> ef])
           `(let ([,var* ,val*] ...) ,ef)]
          [(,prim ,[Effect -> exp*] ...) (guard (memq prim pred-prim))
                                        (make-nopless-begin `(,exp* ...))]
          [(,prim ,[Value -> exp*] ...) (guard (memq prim effect-prim)) 
                                        `(,prim ,exp* ...)]
          [(,prim ,[Effect -> exp*] ...) (guard (memq prim value-prim)) 
                                        (make-nopless-begin `(,exp* ...))]
          [(,[Value -> exp] ,[Value -> exp*] ...) `(,exp ,exp* ...)]
          [,x (error who "invalid Effect ~s" x)])))

    (lambda (x)
      (match x
        [(letrec ([,label* (lambda ,uvar* ,[Value -> body*])] ...) ,[Value -> body])
         `(letrec ([,label* (lambda ,uvar* ,body*)] ...) ,body)]
        [,x (error who "invalid Prog ~s" x)])))
  )

