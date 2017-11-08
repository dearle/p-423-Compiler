(library 
 (Compiler optimize-source)
 (export optimize-source)
 (import
  (chezscheme)
  (Framework match)
  (Framework helpers))
 
 (define-who optimize-source
   
   (define primitives
     '(+ - * <= < = >= > boolean? car cdr cons eq? fixnum? make-vector null? 
          pair? set-car! set-cdr! vector? vector-length vector-ref vector-set! 
          procedure? make-procedure procedure-code procedure-ref procedure-set! void))
   
   (define effect-prims
     '(set-car! set-cdr! vector-set! procedure-set!))
   
   (define prim?
      (lambda (x)
        (or (memv x '(+ - * < <= = >= >)))))
   
   (define (fixnum? x)
     (and (and (integer? x) (exact? x))
          (fixnum-range? x)))       

   (define orlist
     (lambda (ls)
       (cond
         [(null? ls) #f]
         [(= (length ls) 1) (car ls)]
         [else
          (let ([tmp (car ls)])
            (if tmp
                tmp
                (orlist (cdr ls))))])))
   
   (define (constant? x)
     (or (memq x '(#t #f ()))
         (and (and (integer? x) (exact? x))
              (or (fixnum-range? x)
                  (error who "integer ~s is out of fixnum range" x)))))
   
   (define Expr
     (lambda (env)
       (lambda (x)
         (match x
           [,uvar (guard (uvar? uvar)) (let ([test (assq uvar env)])
                                         (if test
                                             (values (cdr test) (list (cdr test)) #f) 
                                             (values uvar `(,uvar) #f)))]
           [,label (guard (label? label)) (values label '() #t)]
           [(quote ,x) (values `(quote ,x) '() #f)]
           [(* (quote ,x) (quote ,y)) (guard (constant? x) (constant? y))
                                      (if (fixnum-range? (* x y))
                                          (values `(quote ,(* x y)) '() #f)
                                          (values x '() #f))]
           [(,op (quote ,x) (quote ,y)) (guard (and (prim? op) (constant? x) (constant? y)))
                                        (values `(quote ,(eval `(,op ,x ,y))) env #f)]
           [(let ([,uvar* ,[(Expr env) -> expr* ref* use*]] ...) ,exp)
            (let ([new-env (append (filter (lambda (ls)
                                             (or (uvar? (cdr ls))
                                                 (if (list? (cdr ls)) 
                                                     (eq? (cadr ls) 'quote)
                                                     #f)
                                                 #f)) (map (lambda (x y)
                                                                  (cons x y)) uvar* expr*)) env)])
              (let-values ([(expr ref use) ((Expr new-env) exp)])
                (let* ([useless (difference uvar* ref)]
                       [new-binds (filter (lambda (ls)
                                           (not (memq (car ls) useless)))
                                         `([,uvar* ,expr*] ...))])
                  (if (null? new-binds)
                      (values expr ref use)
                      (values `(let ,new-binds ,expr) (union (apply union ref*) ref)
                          (or (orlist use*) use))))))]
           [(letrec ([,label* ,[(Expr env) -> expr* ref* use*]] ...) ,[(Expr '()) -> expr ref use])
            (values `(letrec ([,label* ,expr*] ...) ,expr) (union (apply union ref*) ref) 
                    (or (orlist use*) use))]
           [(lambda (,uvar* ...) ,exp)
            (let-values ([(expr refd-vars useful) ((Expr env) exp)])
              (values `(lambda (,uvar* ...) ,expr) refd-vars useful))]
           [(if ,[(Expr env) -> test-expr test-ref test-use] ,conseq ,altern)
            (cond
              [(and (pair? test-expr) (pair? (cdr test-expr)) (eq? (cadr test-expr) #f)) 
               (let-values ([(alt-expr alt-ref alt-use) ((Expr env) altern)])
                 (values alt-expr alt-ref alt-use))]
              [(and (pair? test-expr) (eq? (car test-expr) 'quote)) 
               (let-values ([(con-expr con-ref con-use) ((Expr env) conseq)])
                 (values con-expr con-ref con-use))]
              [else (let*-values ([(alt-expr alt-ref alt-use) ((Expr env) altern)]
                                  [(con-expr con-ref con-use) ((Expr env) conseq)])
                      (values `(if ,test-expr ,con-expr ,alt-expr) 
                              (union test-ref (union alt-ref con-ref))
                              (or test-use con-use alt-use)))])]
           [(begin ,[(Expr env) -> eff-expr* eff-ref* eff-use*] ... 
                   ,[(Expr env) -> eff-expr eff-ref eff-use])
            (let* ([all-list* (map (lambda (x y z)
                                     `(,x ,y ,z)) eff-expr* eff-ref* eff-use*)]
                   [use-list* (filter (lambda (ls)
                                        (caddr ls)) all-list*)]
                   [expr* (map car use-list*)]
                   [ref* (map cadr use-list*)]
                   [use* (map caddr use-list*)])
              (if (null? use-list*)
                  (values eff-expr eff-ref eff-use)
                  (values (make-begin `(,expr* ... ,eff-expr)) 
                          (union (apply union `(,ref* ...)) eff-ref)
                          (or (orlist `(,use* ...)) eff-use))))]
           [(,prim ,[(Expr env) -> expr* ref* use*] ...) 
            (guard (memq prim primitives))
            (if (memq prim effect-prims)
                (values `(,prim ,expr* ...) (apply union ref*) #t)
                (values `(,prim ,expr* ...) (apply union ref*) (orlist use*)))]
           [(,[(Expr env) -> expr ref use] ,[(Expr env) -> rand-expr* rand-ref* rand-use*] ...) 
            (values `(,expr ,rand-expr* ...) (union (apply union rand-ref*) ref) 
                    (or (orlist rand-use*) #t))]
           [,x (error who "invalid Expr ~s" x)]))))
     
     (lambda (x)
       (let-values ([(expr refd-vars useful) ((Expr '()) x)])
         expr)))
   )