(library 
 (Compiler specify-representation)
 (export specify-representation)
 (import
  (chezscheme)
  (Framework match)
  (Framework helpers)
  (Framework prims))
 
 (define-who specify-representation
   
   (define Primitives
     (lambda (prim arg*)
       (let ([offset-car (- disp-car tag-pair)]
             [offset-cdr (- disp-cdr tag-pair)])
         (case prim
           [(+ - < <= = >= >) (let ([x (Value (car arg*))]
                                    [y (Value (cadr arg*))])
                                `(,prim ,x ,y))]
           [(*) (match arg*
                  [((quote ,x) ,y) `(,prim ,(Value y) ,x)]
                  [(,x (quote ,y)) `(,prim ,(Value x) ,y)]
                  [(,x ,y) `(,prim ,(Value x) (sra ,(Value y) ,shift-fixnum))])]
           [(cons) (let ([x (Value (car arg*))]
                         [y (Value (cadr arg*))]
                         [tmp-car (unique-name 't)]
                         [tmp-cdr (unique-name 't)]
                         [tmp (unique-name 't)])
                     `(let ([,tmp-car ,x] [,tmp-cdr ,y])
                        (let ([,tmp (+ (alloc ,size-pair) ,tag-pair)])
                          (begin
                            (mset! ,tmp ,offset-car ,tmp-car)
                            (mset! ,tmp ,offset-cdr ,tmp-cdr)
                            ,tmp))))]
           [(car cdr) (let ([x (Value (car arg*))])
                        (if (eq? prim 'car)
                            `(mref ,x ,offset-car)
                            `(mref ,x ,offset-cdr)))]
           [(set-car! set-cdr!) (let ([x (Value (car arg*))]
                                      [y (Value (cadr arg*))])
                                  (if (eq? prim 'set-car!)
                                      `(mset! ,x ,offset-car ,y)
                                      `(mset! ,x ,offset-cdr ,y)))]
           [(make-vector) (let ([x (Value (car arg*))]
                                [tmp-vctr (unique-name 't)])
                            `(let ([,tmp-vctr (+ (alloc (+ ,disp-vector-data ,x)) ,tag-vector)])
                               (begin
                                 (mset! ,tmp-vctr ,(- disp-vector-length tag-vector) ,x)
                                 ,tmp-vctr)))]
           [(make-procedure) (let ([x (Value (car arg*))]
                                   [y (Value (cadr arg*))]
                                   [tmp-proc (unique-name 't)])
                               `(let ([,tmp-proc (+ (alloc (+ ,disp-procedure-data ,y)) 
                                                    ,tag-procedure)])
                                  (begin
                                    (mset! ,tmp-proc ,(- disp-procedure-code tag-procedure) ,x)
                                    ,tmp-proc)))]
           [(vector-length) (let ([x (Value (car arg*))])
                              `(mref ,x ,(- disp-vector-length tag-vector)))]
           [(procedure-code) (let ([x (Value (car arg*))])
                              `(mref ,x ,(- disp-procedure-code tag-procedure)))]
           [(vector-set!) (let ([x (Value (car arg*))]
                                [y (Value (cadr arg*))]
                                [z (Value (caddr arg*))])
                            (match y
                              [,y (guard (fixnum? y))
                                `(mset! ,x ,(+ (- disp-vector-data tag-vector) y) ,z)]
                              [,y `(mset! ,x (+ ,(- disp-vector-data tag-vector) ,y) ,z)]))]
           [(procedure-set!) (let ([x (Value (car arg*))]
                                   [y (Value (cadr arg*))]
                                   [z (Value (caddr arg*))])
                               (match y
                                 [,y (guard (fixnum? y))
                                  `(mset! ,x ,(+ (- disp-procedure-data tag-procedure) y) ,z)]
                                 [,y 
                                  `(mset! ,x (+ ,(- disp-procedure-data tag-procedure) ,y) ,z)]))]
           [(vector-ref) (let ([x (Value (car arg*))]
                               [y (Value (cadr arg*))])
                           (match y
                              [,y (guard (fixnum? y))
                                `(mref ,x ,(+ (- disp-vector-data tag-vector) y))]
                              [,y `(mref ,x (+ ,(- disp-vector-data tag-vector) ,y))]))]
           [(procedure-ref) (let ([x (Value (car arg*))]
                                  [y (Value (cadr arg*))])
                              (match y
                                [,y (guard (fixnum? y))
                                 `(mref ,x ,(+ (- disp-procedure-data tag-procedure) y))]
                                [,y 
                                 `(mref ,x (+ ,(- disp-procedure-data tag-procedure) ,y))]))]
           [(eq?) (let ([x (Value (car arg*))]
                        [y (Value (cadr arg*))])
                    `(= ,x ,y))]
           [(null?) (let ([x (Value (car arg*))])
                      `(= ,x ,$nil))]
           [(boolean?) (let ([x (Value (car arg*))])
                         `(= (logand ,x ,mask-boolean) ,tag-boolean))]
           [(fixnum?) (let ([x (Value (car arg*))])
                        `(= (logand ,x ,mask-fixnum) ,tag-fixnum))]
           [(pair?) (let ([x (Value (car arg*))])
                      `(= (logand ,x ,mask-pair) ,tag-pair))]
           [(vector?) (let ([x (Value (car arg*))])
                        `(= (logand ,x ,mask-vector) ,tag-vector))]
           [(procedure?) (let ([x (Value (car arg*))])
                        `(= (logand ,x ,mask-procedure) ,tag-procedure))]))))
   
   (define Value
     (lambda (value)
       (match value
         ['#f $false] ;;can also use '(quote #f)
         ['#t $true]
         [(void) $void]
         ['() $nil]
         [(quote ,num) (guard (and (integer? num) (exact? num)))
                       (ash num shift-fixnum)]
         [,label (guard (label? label)) label]
         [,uvar (guard (uvar? uvar)) uvar]
         [(if ,(Pred -> test) ,(Value -> conseq) ,(Value -> altern))
          `(if ,test ,conseq ,altern)]
         [(begin ,[Effect -> eff*] ... ,(Value -> val)) 
          (make-begin `(,eff* ... ,val))]
         [(let ([,uvar* ,(Value -> val*)] ...) ,(Value -> val))
          `(let ([,uvar* ,val*] ...) ,val)]
         [(,prim ,arg* ...) 
          (guard (or (checkValPrim prim) 
                     (memq prim '(make-procedure procedure-code procedure-ref))))
          (Primitives prim arg*)]
         [(,(Value -> val) ,(Value -> val*) ...) 
          `(,val ,val* ...)]
         [,val (error who "invalid Value ~s" val)])))
   
   (define Effect
     (lambda (ef)
       (match ef
         [(nop) '(nop)]
         [(if ,(Pred -> test) ,(Effect -> conseq) ,(Effect -> altern))
          `(if ,test ,conseq ,altern)]
         [(begin ,(Effect -> eff*) ... ,(Effect -> eff))
          (make-begin `(,eff* ... ,eff))]
         [(let ([,uvar* ,(Value -> val*)] ...) ,(Effect -> eff))
          `(let ([,uvar* ,val*] ...) ,eff)]
         [(,prim ,arg* ...) 
          (guard (or (checkEffectPrim prim)
                     (memv prim '(procedure-set!))))
          (Primitives prim arg*)]
         [(,(Value -> val) ,(Value -> val*) ...) 
          `(,val ,val* ...)]
         [,ef (error who "invalid Effect ~s" ef)])))
   
   (define Pred
     (lambda (pr)
       (match pr
         [(true) '(true)]
         [(false) '(false)]
         [(if ,(Pred -> test) ,(Pred -> conseq) ,(Pred -> altern))
          `(if ,test ,conseq ,altern)]
         [(begin ,(Effect -> eff*) ... ,(Pred -> pred)) 
          (make-begin `(,eff* ... ,pred))]
         [(let ([,uvar* ,(Value -> val*)] ...) ,(Pred -> pred))
          `(let ([,uvar* ,val*] ...) ,pred)]
         [(,prim ,arg* ...) 
          (guard (or (checkPredPrim prim) (memq prim '(procedure?)))) 
                 (Primitives prim arg*)]
         [,pr (error who "invalid Pred ~s" pr)])))
   
   
   (lambda (x)
     (match x
       [(letrec ([,label* (lambda (,uvar* ...) ,(Value -> val*))] ...) ,(Value -> val))
        `(letrec ([,label* (lambda (,uvar* ...) ,val*)] ...) ,val)]
       [,x (error who "invalid Program ~s" x)])))
 )

