#lang racket
(require "statements.rkt"
         "error-checker.rkt"
         "utils.rkt")
(provide compile-fn)

;; helper function that creates a list of A-PRIMPL constants
;; having unique IDs created by adding the var-name [an element in
;; vars] to the end of the function name [fn is a function that generates this
;; ID from the var-name]. The values of the constants begin
;; with [i] and are incremented with each recursive call
(define (gen-vars-offs/h fn vars i)
  (cond
    [(empty? vars) empty]
    [else
     (define var (first vars))
     (define var-lbl (fn var))
     (cons `(const ,var-lbl ,i)
           (gen-vars-offs/h fn (rest vars) (+ 1 i)))]))

;; generates the A-PRIMPL instructions that create offsets for
;; the function variables. [vars] is a list of variable names
;; and [fn] is a function that takes in a string and produces
;; a symbol with the string appended to the function name
(define (gen-vars-offs fn vars)
  (gen-vars-offs/h fn vars 2))

;; generates the A-PRIMPL instructions that create offsets for
;; the function arguments. [args] is a list of argument names
;; and [fn] is a function that takes in a string and produces
;; a symbol with the string appended to the function name
(define (gen-args-offs fn args)
  (gen-vars-offs/h fn args (* -1 (length args))))

;; converts the given SIMPL-F function [fun] into a list of A-PRIMPL instructions
(define (compile-fn fun)
  (match fun
    [`(fun (,fname ,args ...) (vars [,vars ...] ,stmts ...))
     (define fn-label (append-fn fname))
     (define extend-fn-label (lambda (x) (extend-symbol fn-label x)))
     (define vars-list (map first vars))
     (error-checker vars-list args stmts)
     (define fp-offsets
       (append (gen-args-offs extend-fn-label args)
               `((const ,(extend-fn-label "FP") 0) 
                 (const ,(extend-fn-label "RETURN-ADDR") 1))
               (gen-vars-offs extend-fn-label vars-list)
               `((const ,(extend-fn-label "SIZE") ,(+ 2 (length vars))))))
     (define var-initialization
       (map (lambda (v) `(move (,(extend-fn-label (first v)) SP) ,(second v))) vars))
     (define prologue
       (append `((move (,(extend-fn-label "FP") SP) FP)
                 (move (,(extend-fn-label "RETURN-ADDR") SP) RETURN-ADDR))
               var-initialization
               `((move FP SP)
                 (add SP SP ,(extend-fn-label "SIZE")))))
     (define epilogue
       `((sub SP SP ,(extend-fn-label "SIZE"))
         (move FP (,(extend-fn-label "FP") SP))
         (move RETURN-ADDR (,(extend-fn-label "RETURN-ADDR") SP))
         (jump RETURN-ADDR)))
     (append `((label ,fn-label))
             fp-offsets
             prologue
             (compile-seq compile-stmt stmts extend-fn-label)
             epilogue)]))
