#lang racket
(require "arithmetic-expressions.rkt" 
         "boolean-expressions.rkt" 
         "utils.rkt")
(provide compile-stmt)

;; converts a given SIMPL statement [stmt] into a list of A-PRIMPL instructions
;; [fn] is a function that takes in a string and produces a symbol with
;; the given string added to the end of the function name
(define (compile-stmt stmt fn)
  (match stmt
    [`(print ,v)
     (if (string? v)
         `((print-string ,(string-append "\"" v "\"")))
         (append (compile-aexp v fn) 
                 `((print-val (-1 SP)) 
                   (sub SP SP 1))))]
    [`(set ,id ,ae) 
     (append (compile-aexp ae fn) 
             `((move (,(fn id) FP) (-1 SP)) 
               (sub SP SP 1)))]
    [`(seq ,stmts ...) (compile-seq compile-stmt stmts fn)]
    [`(iif ,be ,tb ,fb)
     (define LABEL0 (generate-label))
     (define LABEL1 (generate-label))
     (define LABEL2 (generate-label))
     (append (compile-bexp be fn)
             `((sub SP SP 1) 
               (branch (0 SP) ,LABEL0) 
               (jump ,LABEL1) 
               (label ,LABEL0))
             (compile-stmt tb fn)
             `((jump ,LABEL2) 
               (label ,LABEL1))
             (compile-stmt fb fn)
             `((label ,LABEL2)))]
    [`(skip) empty]
    [`(while ,be ,stmts ...)
     (define LABEL0 (generate-label))
     (define LABEL1 (generate-label))
     (define LABEL2 (generate-label))
     (append `((label ,LABEL0))
             (compile-bexp be fn)
             `((sub SP SP 1) 
               (branch (0 SP) ,LABEL1) 
               (jump ,LABEL2) 
               (label ,LABEL1))
             (compile-seq compile-stmt stmts fn)
             `((jump ,LABEL0) 
               (label ,LABEL2)))]
    [`(return ,ae) (append (compile-aexp ae fn) 
                           `((move RETURN-VAL (-1 SP)) 
                             (sub SP SP 1)))]))
