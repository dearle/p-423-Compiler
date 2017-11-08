(library 
 (Compiler optimize-jumps)
 (export optimize-jumps)
 (import (chezscheme)
         (Framework helpers)
         (Framework match))
 
 (define-who optimize-jumps
       
   (define find-and-fix-jumps
     (lambda (x*)
       (let* ([jmp-ls '()]
              [binds (filter (lambda (ls)
                               (if (and (label? (car ls)) (label? (caar (cddadr ls))))
                                   (begin
                                     (set! jmp-ls (cons (cons (car ls) (caar (cddadr ls))) jmp-ls))
                                     #f)
                                   #t)) x*)])
           (values jmp-ls binds))))
   
   (define cycle-check-prep
     (lambda (jmp-ls)
       
       (define cycle-var '())
       (define ref jmp-ls)
        
       (define cycle-check
         (lambda (jmp-ls)
         
           (define visited cycle-var)
           
           (define find-cycle
             (lambda (ls)
               (begin 
                 (set! visited (union `(,(car ls)) visited))
                 (if (memq (cdr ls) visited)
                     (begin
                       (set! cycle-var (union cycle-var `(,(cdr ls))))
                       (cons (car ls) (cdr ls)))
                     (if (assq (cdr ls) ref)
                         (begin
                           (set! visited (cons (cdr ls) visited))
                           (find-cycle (cons (car ls) (cdr (assq (cdr ls) ref)))))
                         (cons (car ls) (cdr ls))))))) 
           (match jmp-ls
             [() '()]
             [(,[find-cycle -> x] ,y* ...) (cons x (cycle-check (cdr jmp-ls)))])))
       
       (values (cycle-check jmp-ls) (map (lambda (x)
                                           `(,x (lambda () (,x)))) cycle-var))))
   
   (define Program
     (lambda (prog jmp-ls)
       (match prog
         [(letrec ((,label* (lambda () ,[(Tail jmp-ls) -> tail*])) ...)  ,[(Tail jmp-ls) -> tail])
          `(letrec ((,label* (lambda () ,tail*)) ...)  ,tail)])))
   
   (define Tail
     (lambda (jmp-ls)
       (lambda (tail)
         (match tail
           [(,[(Triv jmp-ls) -> triv]) `(,triv)]
           [(if (,relop ,[(Triv jmp-ls) -> triv1] ,[(Triv jmp-ls) -> triv2]) 
                (,[(Triv jmp-ls) -> label1]) (,[(Triv jmp-ls) -> label2])) 
            `(if (,relop ,triv1 ,triv2) (,label1) (,label2))]
           [(begin ,[(Effect jmp-ls) -> eff*] ... ,[(Tail jmp-ls) -> tail*]) 
            `(begin ,eff* ... ,tail*)]
           [,x x]))))
   
   (define Effect
     (lambda (jmp-ls)
       (lambda (ef)
         (match ef
           [(set! ,loc ,[(Triv jmp-ls) -> triv]) `(set! ,loc ,triv)]
           [(set! ,loc (,binop ,[(Triv jmp-ls) -> triv1] ,[(Triv jmp-ls) -> triv2]))
            `(set! ,loc (,binop ,triv1 ,triv2))]
           [,x x]))))
   
   (define Triv
     (lambda (jmp-ls)
       (lambda (trv)
         (let ([need-change? (assq trv jmp-ls)])
           (if (and (label? trv) need-change?)
               (cdr need-change?)
               trv)))))
   
   (lambda (x)
     (match x
       [(letrec (,lambda* ...) ,tail)
        (if (null? lambda*)
            `(letrec (,lambda* ...) ,tail)
            (let*-values ([(jmp-ls new-binds) (find-and-fix-jumps lambda*)]
                          [(jmp-ls cycle-binds) (cycle-check-prep jmp-ls)]) 
                (Program `(letrec (,new-binds ... ,cycle-binds ...) ,tail) jmp-ls)))]
       [,x (error who "invalid Program ~s" x)])))
 )