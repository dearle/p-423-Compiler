(library 
 (Compiler parse-scheme)
 (export parse-scheme)
 (import
  (chezscheme)
  (Framework match)
  (Framework helpers))
 
 (define-who parse-scheme
   
   (define primitives
     '((+ . 2) (- . 2) (* . 2) (<= . 2) (< . 2) (= . 2)
               (>= . 2) (> . 2) (boolean? . 1) (car . 1) (cdr . 1)
               (cons . 2) (eq? . 2) (fixnum? . 1) (make-vector . 1)
               (null? . 1) (pair? . 1) (procedure? . 1) (set-car! . 2)
               (set-cdr! . 2) (vector? . 1) (vector-length . 1)
               (vector-ref . 2) (vector-set! . 3) (void . 0)))
   
   
   (define (constant? x)
     (or (memq x '(#t #f ()))
         (and (and (integer? x) (exact? x))
              (or (fixnum-range? x)
                  (error who "integer ~s is out of fixnum range" x)))))
   
   (define (datum? x)
     (or (constant? x)
         (if (pair? x)
             (and (datum? (car x)) (datum? (cdr x)))
             (and (vector? x) (andmap datum? (vector->list x))))))
   
   (define verify-x-list
     (lambda (x* x? what)
       (let loop ([x* x*] [idx* '()])
         (unless (null? x*)
           (let ([x (car x*)] [x* (cdr x*)])
             (unless (x? x)
               (error who "invalid ~s ~s found" what x))
             (let ([idx (extract-suffix x)])
               (when (member idx idx*)
                 (error who "non-unique ~s suffix ~s found" what idx))
               (loop x* (cons idx idx*))))))))
   
   (define Replace-vars 
     (lambda (var* env)
       (match var*
         [() '()]
         [,x (guard (assq x env)) (cdr (assq x env))]
         [(,[x] . ,[y]) `(,x . ,y)]
         [,x (error who "var not replaced: ~s" x )])))
   
   (define Program
     (lambda (x)
       
       (define all-uvar* '())
       
       (define Expr
         (lambda (env uvar*)
           (lambda (x)
             (match x
               [,x (guard (constant? x))
                   `(quote ,x)]
               [,x (guard (symbol? x))
                    (if (assq x env)
                        (cdr (assq x env))
                        (error who "unbound variable ~s" x))]
               [(,rator ,rand* ...) (guard (assq rator env))
                                    (map (Expr env uvar*) `(,rator ,rand* ...))]
               [(quote ,x)
                (unless (datum? x) (error who "invalid datum ~s" x))
                `(quote ,x)]
               [(if ,[(Expr env uvar*) -> test] ,[(Expr env uvar*) -> conseq])
                `(if ,test ,conseq (void))]
               [(if ,[(Expr env uvar*) -> test] ,[(Expr env uvar*) -> conseq] ,[(Expr env uvar*) -> altern])
                `(if ,test ,conseq ,altern)]
               [(begin ,[(Expr env uvar*) -> x*] ... ,[(Expr env uvar*) -> x])
                `(begin ,x* ... ,x)]
               [(lambda (,fml* ...) ,x ,x* ...) 
                (guard (for-all symbol? fml*))
                (let* ([new-env (map (lambda (fml)
                                    (cons fml (unique-name fml))) fml*)]
                       [new-fml (Replace-vars fml* new-env)])
                  (set! all-uvar* (append new-fml all-uvar*))
                  `(lambda (,new-fml ...)
                     (begin
                       ,@(map (Expr `(,(append new-env env) ...) (append new-fml uvar*)) (cons x x*)))))]
               [(let ([,new-uvar* ,[(Expr env uvar*) -> x*]] ...) ,x ,x** ...)
                (let* ([new-env (map (lambda (var)
                                       (cons var (unique-name var))) new-uvar*)]
                       [new-uvars (Replace-vars new-uvar* new-env)])
                  (set! all-uvar* (append new-uvars all-uvar*))
                  `(let ([,new-uvars ,x*] ...)
                     ,(let* ([new-env (append new-env env)]
                             [new-uvars (append new-uvars uvar*)])
                        `(begin ,@(map (Expr new-env new-uvars) (cons x x**))))))]
               [(letrec ([,new-uvar* ,x*] ...) ,x ,x** ...)
                (let* ([new-env (map (lambda (var)
                                       (cons var (unique-name var))) new-uvar*)]
                       [new-uvars (Replace-vars new-uvar* new-env)])
                  (set! all-uvar* (append new-uvars all-uvar*))
                  (let* ([new-env (append new-env env)]
                         [new-uvar* (append new-uvars uvar*)])
                    `(letrec ([,new-uvars ,(map (Expr new-env new-uvar*) x*)] ...)
                       (begin ,@(map (Expr new-env new-uvar*) (cons x x**))))))]
               [(set! ,uvar ,[(Expr env uvar*) -> x])
                (if (assq uvar env)
                    `(set! ,(Replace-vars uvar env) ,x)
                    (error who "unbound uvar ~s" uvar))]
               [(not ,[(Expr env uvar*) -> x]) 
                `(if ,x '#f '#t)]
               [(and) '#t]
               [(and ,[(Expr env uvar*) -> x]) x]
               [(and ,[(Expr env uvar*) -> x] ,x* ...)
                `(if ,x ,((Expr env uvar*) `(and ,x* ...)) '#f)]
               [(or) '#f]
               [(or ,[(Expr env uvar*) -> x]) x]
               [(or ,[(Expr env uvar*) -> x] ,x* ...)
                (let ([tmp (unique-name who)])
                  `(let ([,tmp ,x])
                     (if ,tmp ,tmp ,((Expr env uvar*) `(or ,x* ...)))))]
               [(,prim ,[(Expr env uvar*) -> x*] ...)
                (guard (assq prim primitives))
                (unless (= (length x*) (cdr (assq prim primitives)))
                  (error who "too many or few arguments ~s for ~s" (length x*) prim))
                `(,prim ,x* ...)]
               [(,rator ,rand* ...)
                (map (Expr env uvar*) `(,rator ,rand* ...))]
               [,x (error who "invalid Expr ~s" x)]))))
       
       (let ([x ((Expr '() '()) x)])
         (verify-x-list all-uvar* uvar? 'uvar)
         x)))
   
   (lambda (x) 
     (Program x)))
 )