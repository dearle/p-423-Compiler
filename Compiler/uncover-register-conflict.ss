(library 
 (Compiler uncover-register-conflict)
 (export uncover-register-conflict)
 (import
  (chezscheme)
  (Framework helpers)
  (Framework match))

 (define-who uncover-register-conflict
   
   (define Body
     (lambda (x)
       
       (define warn-if-dead-at-assignment (make-parameter #f))
       
       (define do-uncover-conflict
         (lambda (tail uvar*)
           
           (define add-conflicts!
             (lambda (ct lhs live*)
               
               (define add-conflict!
                 (lambda (var1 var2)
                   (let ([a (assq var1 ct)])
                     (set-cdr! a (set-cons var2 (cdr a))))))
               
               (when (uvar? lhs)
                 (for-each
                  (lambda (live) (add-conflict! lhs live))
                  live*))
               (for-each
                (lambda (live) (when (uvar? live) (add-conflict! live lhs)))
                live*)))
           
           (define Triv (lambda (x) (if (or (uvar? x) (register? x)) `(,x) '())))
           
           (define Effect*
             (lambda (x live* ct)
               (match x
                 [() live*]
                 [(,ef* ... ,ef) (Effect* ef* (Effect ef live* ct) ct)]
                 [,x (error who "invalid Effect* list ~s" x)])))
           
           (define Effect
             (lambda (x live* ct)
               (match x
                 [(nop) live*]
                 [(if ,test ,[c-live*] ,[a-live*]) (Pred test c-live* a-live* ct)]
                 [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
                 [(set! ,lhs ,rhs)
                  (guard (or (uvar? lhs) (register? lhs)) (not (memq lhs live*)))
                  (when (warn-if-dead-at-assignment)
                    (warning who "~s is not live at assignment ~s" lhs
                             `(set! ,lhs ,rhs)))
                  (Effect `(set! ,lhs ,rhs) (cons lhs live*) ct)]
                 [(set! ,lhs (,binop ,[Triv -> x-live*] ,[Triv -> y-live*]))
                  (let ([live* (difference live* `(,lhs))])
                    (when (or (uvar? lhs) (register? lhs))
                      (add-conflicts! ct lhs live*))
                    (union x-live* y-live* live*))]
                 [(set! ,lhs ,var)
                  (let ([live* (difference live* `(,lhs))])
                    (when (or (uvar? lhs) (register? lhs))
                      (add-conflicts! ct lhs (remq var live*)))
                    (union (Triv var) live*))]
                 [(mset! ,[Triv -> base-expr] ,[Triv -> offset-expr] ,[Triv -> expr])
                    (union base-expr offset-expr expr live*)]
                 [(return-point ,rp-label ,tail)
                  (Tail tail ct)]
                 [,x (error who "invalid Effect list ~s" x)])))
           
           (define Pred
             (lambda (x t-live* f-live* ct)
               (match x
                 [(true) t-live*]
                 [(false) f-live*]
                 [(if ,test ,[c-live*] ,[a-live*]) (Pred test c-live* a-live* ct)]
                 [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
                 [(,relop ,[Triv -> x-live*] ,[Triv -> y-live*])
                  (union t-live* f-live* x-live* y-live*)]
                 [,x (error who "invalid Pred ~s" x)])))
           
           (define Tail
             (lambda (x ct)
               (match x
                 [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
                 [(if ,test ,[c-live*] ,[a-live*]) (Pred test c-live* a-live* ct)]
                 [(,[Triv -> target-live*] ,live* ...)
                  (union (union target-live*
                         (filter
                          (lambda (x) (or (register? x) (uvar? x)))
                          live*)))]
                 [,x (error who "invalid Tail ~s" x)])))
           (let ([ct (map (lambda (x) (cons x '())) uvar*)])
             (let ([uvar* (filter uvar? (Tail tail ct))])
               (unless (null? uvar*)
                 (warning who "found variables ~s live on entry" uvar*)))
             ct)))

       (match x
         [(locals (,local* ...)
                  (ulocals (,ulocal* ...)
                           (locate (,home* ...)
                                   (frame-conflict ,fv-ct ,tail))))
          ;; setup the conflict table ct for storing conflicts
          (let ([ct (do-uncover-conflict tail `(,local* ... ,ulocal* ...))])
            `(locals (,local* ...)
                     (ulocals (,ulocal* ...)
                              (locate (,home* ...)
                                      (frame-conflict ,fv-ct
                                                      (register-conflict ,ct ,tail))))))]
         [(locate (,home* ...) ,tail) `(locate (,home* ...) ,tail)]
         [,x (error who "invalid Body ~s" x)])))
   
   (lambda (x)
     (match x
       [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
        `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
       [,x (error who "invalid Program ~s" x)])))
 )