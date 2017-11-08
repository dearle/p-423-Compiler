(library (Compiler uncover-free)
  (export uncover-free)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers))

(define-who uncover-free
  
  (define primitives
    '(+ - * <= < = >= > boolean? car cdr cons eq? fixnum? make-vector null? pair? procedure?
        set-car! set-cdr! vector? vector-length vector-ref vector-set! void))

  (define setify
    (lambda (ls1 ls2)
      (if (null? ls1)
          ls1
          (if (memq (car ls1) ls2)
              (setify (cdr ls1) ls2)
              (cons (car ls1) (setify (cdr ls1) (cons (car ls1) ls2)))))))
  
  (define Lambda
    (lambda (x)
      (match x
        [(lambda (,bound-vars* ...) ,[Expr -> exp free-ls])
         (let ([free-list (difference free-ls bound-vars*)])
           (values `(lambda (,bound-vars* ...) (free ,free-list ,exp)) free-list))]
        [,x (error who "invalid Lambda ~s" x)])))

  (define Expr
      (lambda (x)
        (match x
          [,uvar (guard (uvar? uvar)) (values uvar `(,uvar))]
          [(quote ,x) (values `(quote ,x) '())]
          [(if ,test ,conseq ,altern)
           (let-values ([(new-test free-ls1) (Expr test)]
                        [(new-conseq free-ls2) (Expr conseq)]
                        [(new-altern free-ls3) (Expr altern)])
             (values `(if ,new-test ,new-conseq ,new-altern) 
                     (union free-ls1 free-ls2 free-ls3)))]
          [(begin ,[Expr -> ef* free-ls*] ... ,ef)
           (let-values ([(new-ef free-ls) (Expr ef)])
             (values `(begin ,ef* ... ,new-ef) 
                     (union (setify `(,free-ls* ... ...) '()) free-ls)))]
          [(let ([,new-uvar* ,[Expr -> exp* free-ls*]] ...) ,[Expr -> exp free-ls])
           (values `(let ([,new-uvar* ,exp*] ...) ,exp) 
                   (difference (union (setify `(,free-ls* ... ...) '()) free-ls) 
                               new-uvar*))]
          [(letrec ([,new-uvar* ,[Lambda -> lambda* free-ls*]] ...) ,[Expr -> x free-ls])
           (values `(letrec ([,new-uvar* ,lambda*] ...) ,x) 
                   (difference (union (setify `(,free-ls* ... ...) '()) free-ls) 
                               new-uvar*))]
          [(,prim ,[Expr -> x* free-ls*] ...) (guard (memq prim primitives))
                                             (values `(,prim ,x* ...) 
                                                     (setify `(,free-ls* ... ...) '()))]
          [(,[Expr -> exp free-ls] ,[Expr -> exp* free-ls*] ...)
           (values `(,exp ,exp* ...) (union (setify `(,free-ls* ... ...) '()) free-ls))]
          [,x (error who "invalid Expr ~s" x)])))
  
  (lambda (x)
    (let-values ([(new-x free-ls) (Expr x)])
      new-x)))
)

