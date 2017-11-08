(library 
 (Compiler assign-new-frame)
 (export assign-new-frame)
 (import
  (chezscheme)
  (Framework helpers)
  (Framework match))
 
 (define-who assign-new-frame
   
   (define Body
     (lambda (body)
       
       (define frame-bytes 0)
       
       (define find-homes
         (lambda (var-ls home* size)
           (if (null? var-ls)
               home*
               (let ([var (car var-ls)] [var-ls (cdr var-ls)])
                 (find-homes var-ls `((,var ,(index->frame-var size)) . ,home*) (add1 size)))))) 
       
       (define get-frame
         (lambda (var* home* size)
           (if (null? var*)
               home*
               (let ([var-ls (car var*)] [var* (cdr var*)])
                 (let ([homes (find-homes var-ls home* size)])
                   (get-frame var* homes size))))))
       
       (define find-frame-size
         (lambda (c-live* home*)
           (add1 (apply max (map (lambda (var)
                             (if (uvar? var)
                                 (frame-var->index (cadr (assq var home*)))
                                 (frame-var->index var))) c-live*)))))
       
       (define Tail
         (lambda (tail)
           (match tail
             [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
             [(begin ,[Effect -> eff*] ... ,[tail]) `(begin ,eff* ... ,tail)]
             [,x x])))
       
       (define Pred
         (lambda (pred)
           (match pred
             [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
             [(begin ,[Effect -> eff*] ... ,[pred]) `(begin ,eff* ... ,pred)]
             [,x x])))
       
       (define Effect
         (lambda (eff)
           (match eff
             [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
             [(begin ,[eff*] ... ,[eff]) `(begin ,eff* ... ,eff)]
             [(return-point ,rp-label ,tail)
              (make-begin `((set! ,frame-pointer-register (+ ,frame-pointer-register ,frame-bytes))
                            (return-point ,rp-label ,tail)
                            (set! ,frame-pointer-register (- ,frame-pointer-register ,frame-bytes))))]
             [,x x])))
       
       
       (match body
         [(locals (,local* ...)
                  (new-frames ,nfv*
                              (locate ,home*
                                      (frame-conflict ,ct 
                                                      (call-live ,c-live* ,tail)))))
          (let* ([frame-size (if (null? c-live*)
                                 1
                                 (find-frame-size c-live* home*))]
                 [home* (get-frame nfv* home* frame-size)])
            (set! frame-bytes (* word-size frame-size))
            (let ([new-tail (Tail tail)])
              `(locals ,(difference local* (apply append nfv*))
                       (ulocals ()
                                (locate (,home* ...)
                                        (frame-conflict ,ct ,new-tail))))))]
         [,x (error who "invalid Body ~s" x)])))
   
   (lambda (x)
     (match x
       [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
        `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
       [,x (error who "invalid Program ~s" x)])))
 
 )
