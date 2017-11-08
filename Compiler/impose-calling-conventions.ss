(library 
 (Compiler impose-calling-conventions)
 (export impose-calling-conventions)
 (import (chezscheme) 
         (Framework helpers) 
         (Framework match))

 (define-who impose-calling-conventions
   
   (define Body 
     (lambda (bd fml*)
       
       (define new-fvars '())
       
       (define Argument-locations 
         (lambda (args)
           (let loop ([args args] [regs parameter-registers] [fv-idx 0])
             (cond
               [(null? args) '()]
               [(null? regs)
                (cons (index->frame-var fv-idx) (loop (cdr args) regs (+ fv-idx 1)))]
               [else (cons (car regs) (loop (cdr args) (cdr regs) fv-idx))]))))
       
       (define Argument-locations-nfv
         (lambda (args)
           (let loop ([args args] [regs parameter-registers])
             (cond
               [(null? args) '()]
               [(null? regs)
                (cons (unique-name 'nfv) (loop (cdr args) regs))]
               [else (cons (car regs) (loop (cdr args) (cdr regs)))]))))
       
       (define triv?
         (lambda (x) 
           (or (uvar? x) (int64? x) (label? x))))
       
       (define Triv 
         (lambda (t) 
           (if (triv? t) 
               t 
               (error who "invalid Triv ~s" t))))
       
       (define Effect 
         (lambda (ef)
           (match ef
             [(nop) '(nop)]
             [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
             [(begin ,[ef*] ... ,[ef]) (make-begin `(,ef* ... ,ef))]
             [(set! ,var (alloc ,[Triv -> triv]))  
              `(set! ,var (alloc ,triv))]
             [(set! ,var (mref ,[Triv -> triv1] ,[Triv -> triv2]))
              `(set! ,var (mref ,triv1 ,triv2))]
             [(set! ,var (,binop ,[Triv -> x] ,[Triv -> y])) 
              (guard (memq binop '(+ - * logand logor sra))) 
              `(set! ,var (,binop ,x ,y))]
             [(set! ,var (,rator ,rand* ...)) 
              (make-begin
               `(,(Effect `(,rator ,rand* ...))
                 (set! ,var ,return-value-register)))]
             [(mset! ,[Triv -> triv1] ,[Triv -> triv2] ,[Triv -> triv3])
              `(mset! ,triv1 ,triv2 ,triv3)]
             [(set! ,var ,[Triv -> tr]) `(set! ,var ,tr)]
             [(,[Triv -> rator] ,[Triv -> rand*] ...)
              (let ([loc* (Argument-locations-nfv rand*)])
                (let ([rand* (reverse rand*)] 
                      [loc-rand* (reverse loc*)]
                      [rp-label (unique-label 'rp-label)])
                  (set! new-fvars (cons (filter uvar? loc*) new-fvars))
                  `(return-point 
                    ,rp-label
                    ,(make-begin
                      `((set! ,loc-rand* ,rand*) ...
                        (set! ,return-address-register ,rp-label)
                        (,rator ,frame-pointer-register 
                                ,return-address-register 
                                ,allocation-pointer-register ,loc* ...))))))]
             [,x (error who "invalid Effect ~s" x)])))
       
       (define Pred 
         (lambda (pr)
           (match pr
             [(true) '(true)]
             [(false) '(false)]
             [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
             [(begin ,[Effect -> ef*] ... ,[pr]) (make-begin `(,ef* ... ,pr))]
             [(,relop ,[Triv -> x] ,[Triv -> y]) `(,relop ,x ,y)]
             [,x (error who "invalid Pred ~s" x)])))
       
       (define Tail 
         (lambda (tail rp)
           (match tail
             [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
             [(begin ,[Effect -> ef*] ... ,[tail]) (make-begin `(,ef* ... ,tail))]
             [(,binop ,[Triv -> x] ,[Triv -> y])
              (guard (memq binop '(+ - * logand logor sra)))
              (make-begin `((set! ,return-value-register (,binop ,x ,y))
                            (,rp ,frame-pointer-register 
                                 ,allocation-pointer-register 
                                 ,return-value-register)))]
             [(alloc ,[Triv -> triv])
              (make-begin `((set! ,return-value-register (alloc ,triv))
                            (,rp ,frame-pointer-register 
                                 ,allocation-pointer-register
                                 ,return-value-register)))]
             [(mref ,[Triv -> triv1] ,[Triv -> triv2]) 
              (make-begin `((set! ,return-value-register (mref ,triv1 ,triv2))
                            (,rp ,frame-pointer-register 
                                 ,allocation-pointer-register
                                 ,return-value-register)))]
             [(,[Triv -> rator] ,[Triv -> rand*] ...)
              (let ([loc* (Argument-locations rand*)])
                (let ([rand* (reverse rand*)] [loc-rand* (reverse loc*)])
                  (make-begin
                   `((set! ,loc-rand* ,rand*) ...
                     (set! ,return-address-register ,rp)
                     (,rator ,return-address-register 
                             ,frame-pointer-register
                             ,allocation-pointer-register
                             ,loc* ...)))))]
             [,tr (guard (triv? tr))
                  (make-begin `((set! ,return-value-register ,tr)
                                (,rp ,frame-pointer-register
                                     ,allocation-pointer-register
                                     ,return-value-register)))]
             [,x (error who "invalid Tail ~s" x)])))
       
       
       (match bd
         [(locals (,local* ...) ,tail)
          (let ([rp (unique-name 'rp)] 
                [fml-loc* (Argument-locations fml*)])
            (let ([tail (Tail tail rp)])
              `(locals (,rp ,fml* ... ,local* ... ,new-fvars ... ...)
                       (new-frames ,new-fvars
                       ,(make-begin
                         `((set! ,rp ,return-address-register)
                           (set! ,fml* ,fml-loc*) ... ,tail))))))]
         [,x (error who "invalid Body ~s" x)])))
   
   (lambda (prog)
     (match prog
       [(letrec ([,label* (lambda (,fml** ...) ,bd*)] ...) ,bd)
        (let ([bd* (map Body bd* fml**)] 
              [bd (Body bd '())])
          `(letrec ([,label* (lambda () ,bd*)] ...) ,bd))]
       [,x (error who "invalid Program ~s" x)])))
 
 )