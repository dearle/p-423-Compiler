(library (Compiler assign-frame)
  (export assign-frame)
  (import
    (chezscheme)
    (Framework helpers)
    (Framework match))

(define-who assign-frame
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
           (ulocals (,ulocal* ...)
             (spills (,spill* ...)
               (locate (,home* ...)
                 (frame-conflict ,ct ,tail)))))
         (let ([home* (find-homes spill* ct home*)])
           `(locals (,local* ...)
              (ulocals (,ulocal* ...)
                (locate (,home* ...)
                  (frame-conflict ,ct ,tail)))))]
        [(locate (,home* ...) ,body) `(locate (,home* ...) ,body)]
        [,body (error who "invalid Body ~s" body)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,x (error who "invalid Program ~s" x)])))

)