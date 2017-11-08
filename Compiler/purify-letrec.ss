(library 
 (Compiler purify-letrec)
 (export purify-letrec)
 (import
  (chezscheme)
  (Framework match)
  (Framework helpers))
 
 (define-who purify-letrec
   
   (define primitives
     '(+ - * <= < = >= > boolean? car cdr cons eq? fixnum? make-vector null? pair? procedure?
         set-car! set-cdr! vector? vector-length vector-ref vector-set! void))
   
   (define foldl
     (lambda (func n ls)
       (cond
         [(null? ls) n]
         [else (foldl func (func n (car ls)) (cdr ls))])))
   
   (define simple?
     (lambda (letrec-vars e)
       (call/cc
        (lambda (root)
        
          (define loop 
            (lambda (letrec-vars in-lam? cc)
              (lambda (e)
                (match e
                  [(quote ,d) #t]
                  [(let ([,uvar* ,[expr*]] ...) (assigned (,asvars* ...) ,[expr])) #t]
                  [(letrec ([,uvar* ,expr*] ...) ,[body])
                   (let* ([new-lvars (union uvar* letrec-vars)]
                          [_ (map (loop new-lvars in-lam? cc) expr*)]) #t)]
                  [(lambda (,uvar* ...) (assigned (,asvars* ...)
                                                  ,[(loop letrec-vars #t cc) -> body])) #t]
                  [(if ,[test] ,[then] ,[else]) #t]
                  [(begin ,[expr*] ... ,[expr]) #t]
                  [(set! ,uvar ,[expr]) #t]
                  [(,prim ,[expr*] ...) (guard (memq prim primitives)) #t]
                  [(,[rator] ,[rand*] ...) (if in-lam?
                                               #t
                                               (cc #f))]
                  [,uvar (guard (uvar? uvar))
                         (if (memq uvar letrec-vars)
                             (cc #f) #t)]
                  [,x (errorf who "Invalid Stuff in simple? ~s" x)]))))
            
            ((loop letrec-vars #f root) e)))))
   
   (define not-assigned?
     (lambda (x as-ls)
       (not (memq x as-ls))))
   
   (define partition
     (lambda (binds as-ls letrec-vars)
       (let loop ([binds binds]
                  [simple '()]
                  [lam '()]
                  [complex '()])
         (cond
           [(null? binds) (values simple lam complex)]
           [else
            (let ([x (caar binds)] [e (cadar binds)])
              (cond
                [(and (not-assigned? x as-ls)
                      (and (pair? e)
                           (eq? 'lambda (car e))))
                 (loop (cdr binds)
                       simple
                       (cons (car binds) lam)
                       complex)]
                [(and (simple? letrec-vars e)
                      (not-assigned? x as-ls))
                 (loop (cdr binds)
                       (cons (car binds) simple)
                       lam
                       complex)]
                [else
                 (loop (cdr binds)
                       simple
                       lam
                       (cons (car binds) complex))]))]))))
   
   (define Expr
     (lambda (x)
       (match x
         [,uvar (guard (uvar? uvar)) uvar]
         [(quote ,d) `(quote ,d)]
         [(let ([,uvar* ,[exp*]] ...) (assigned (,asvars* ...) ,[exp]))
          `(let ([,uvar* ,exp*] ...) (assigned (,asvars* ...) ,exp))]
         [(letrec ([,uvar* ,[exp*]] ...)
            (assigned (,asvars* ...) ,[exp]))
          (let-values ([(simple lam complex) (partition `([,uvar* ,exp*] ...)
                                                        asvars*
                                                        `(,uvar* ...))])
            (let* ([x_c (map car complex)]
                   [e_c (map cadr complex)]
                   [x_t (foldl (lambda (acc var)
                                 (cons (unique-name var) acc))
                               '()
                               x_c)])
              `(let ,simple
                 (assigned ()
                           (let ([,x_c (void)] ...)
                             (assigned (,x_c ...)
                                       (letrec ,lam
                                         ,(make-begin
                                           (if (null? complex)
                                               `(,exp)
                                               `((let ([,x_t ,e_c] ...)
                                                   (assigned ()
                                                             ,(make-begin 
                                                               `((set! ,x_c ,x_t) ...)))) ,exp))))))))))]
         [(lambda (,uvar* ...) (assigned (,asvars* ...) ,[exp]))
          `(lambda (,uvar* ...) (assigned (,asvars* ...) ,exp))]
         [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
         [(begin ,[eff*] ... ,[eff]) `(begin ,eff* ... ,eff)]
         [(set! ,uvar ,[expr]) `(set! ,uvar ,expr)]
         [(,prim ,[exp*] ...) (guard (memq prim primitives)) `(,prim ,exp* ...)]
         [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
         [,x (errorf who "Invalid Expr ~s" x)])))
     
     (lambda (x)
       (Expr x)))
   )
 
 