(library 
 (Compiler convert-complex-datum)
 (export convert-complex-datum)
 (import
  (chezscheme)
  (Framework match)
  (Framework helpers))
 
 (define-who convert-complex-datum
   
   (define primitives
     '(+ - * <= < = >= > boolean? car cdr cons eq? fixnum? make-vector null? pair? procedure?
         set-car! set-cdr! vector? vector-length vector-ref vector-set! void))
   
   (define constant?
     (lambda (x)
       (or (memq x '(#t #f ()))
           (and (and (integer? x) (exact? x))
                (or (fixnum-range? x)
                    (error who "integer ~s is out of fixnum range" x))))))
   
   (define Program
     (lambda (x)
       
       (define let-datum '())
       
       (define Set-Vector
         (lambda (x count tmp)
           (if (null? x)
               '()
               (cons `(vector-set! ,tmp (quote ,count) ,(car x)) 
                     (Set-Vector (cdr x) (add1 count) tmp)))))
       
       (define Process-Datum
         (lambda (x)
           (match x
             [#(,[Process-Datum -> x*] ...)
              (let* ([tmp (unique-name 'tmp)]
                     [set-vects (Set-Vector x* 0 tmp)])
                `(let ([,tmp (make-vector (quote ,(length x*)))])
                   ,(make-begin `(,set-vects ... ,tmp))))]
             [(,[Process-Datum -> x] . ,[Process-Datum -> y]) `(cons ,x ,y)]
             [,x (guard (constant? x)) `(quote ,x)]
             [,x (error who "invalid Datum ~s" x)])))
       
       (define Expr
         (lambda (x)
           (match x
             [,uvar (guard (uvar? uvar)) uvar]
             [(quote ,x) (if (constant? x)
                             `(quote ,x)
                             (let* ([nuvar (unique-name 't)]
                                    [set-datum (Process-Datum x)])
                               (set! let-datum (cons `(,nuvar ,set-datum) let-datum))
                               nuvar))]
             [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
             [(begin ,[eff*] ... ,[eff]) `(begin ,eff* ... ,eff)]
             [(lambda (,uvar* ...) ,[exp]) `(lambda (,uvar* ...) ,exp)]
             [(let ([,uvar* ,[exp*]] ...) ,[exp]) `(let ([,uvar* ,exp*] ...) ,exp)]
             [(letrec ([,uvar* ,[exp*]] ...) ,[exp]) `(letrec ([,uvar* ,exp*] ...) ,exp)]
             [(set! ,uvar ,[exp]) `(set! ,uvar ,exp)]
             [(,prim ,[x*] ...) (guard (memq prim primitives)) `(,prim ,x* ...)]
             [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
             [,x (error who "invalid Expr ~s" x)])))
       
       (let ([exp (Expr x)])
         (if (null? let-datum)
             exp
             `(let ,let-datum ,exp)))))
   
   (lambda (x) 
     (Program x)))
 )

