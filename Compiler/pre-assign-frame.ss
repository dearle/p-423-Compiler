(library 
 (Compiler pre-assign-frame)
 (export pre-assign-frame)
 (import
  (chezscheme)
  (Framework helpers)
  (Framework match))
 
 (define-who pre-assign-frame
   
   (define find-used
     (lambda (conflict* home*)
       (cond
         [(null? conflict*) '()]
         [(frame-var? (car conflict*))
          (set-cons (car conflict*) (find-used (cdr conflict*) home*))]
         [(assq (car conflict*) home*) =>
                                       (lambda (x) (set-cons (cadr x) (find-used (cdr conflict*) home*)))]
         [else (find-used (cdr conflict*) home*)])))
   
   (define find-frame-var
     (lambda (used*)
       (let f ([index 0])
         (let ([fv (index->frame-var index)])
           (if (memq fv used*) (f (+ index 1)) fv)))))
   
   (define find-homes
     (lambda (var* ct home*)
       (if (null? var*)
           home*
           (let ([var (car var*)] [var* (cdr var*)])
             (let ([conflict* (cdr (assq var ct))])
               (let ([home (find-frame-var (find-used conflict* home*))])
                 (find-homes var* ct `((,var ,home) . ,home*))))))))
   
   (define Body
     (lambda (body)
       (match body
         [(locals (,local* ...)
                  (new-frames (,nfv* ...)
                           (spills (,spill* ...)
                                   (frame-conflict ,ct 
                                                   (call-live (,c-live-var* ...) ,tail)))))
          (let ([home* (find-homes spill* ct '())])
            `(locals (,local* ...)
                     (new-frames (,nfv* ...)
                              (locate (,home* ...)
                                      (frame-conflict ,ct 
                                                      (call-live (,c-live-var* ...) ,tail))))))]
         [,body (error who "invalid Body ~s" body)])))
   
   (lambda (x)
     (match x
       [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
        `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
       [,x (error who "invalid Program ~s" x)])))
 
 )
