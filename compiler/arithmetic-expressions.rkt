#lang racket
(require "utils.rkt")
(provide compile-aexp)

;; checks if [op] is one of the known operators in SIMPL
(define (builtin-op? op)
  (member op '(> >= < <= = + - * mod div)))

(define (get-op op)
  (match op
         ['> 'gt]
         ['>= 'ge]
         ['< 'lt]
         ['<= 'le]
         ['= 'equal]
         ['+ 'add]
         ['- 'sub]
         ['* 'mul]
         [x x]))

;; converts a given SIMPL arithmetic expression [ae] into a list of A-PRIMPL
;; instructions
;; [fn] is a function that takes in a string and produces a symbol with
;; the given string added to the end of the function name
(define (compile-aexp ae fn)
  (match ae
    [`(,(? builtin-op? op) ,opds ...)
     (define primpl-op (get-op op))
     (append (compile-seq compile-aexp opds fn)
             `((,primpl-op (-2 SP) (-2 SP) (-1 SP)) 
               (sub SP SP 1)))]
    [`(,fname ,args ...)
     (match (get-fn fname)
       [#f (error "undefined function")]
       [num-args
        (unless (= num-args (length args))
          (error "arguments"))])
     (append (compile-seq compile-aexp args fn)
             `((jsr RETURN-ADDR ,(append-fn fname)) 
               (sub SP SP ,(length args)))
             (stk-push 'RETURN-VAL))]
    [(? number? x) (stk-push x)]
    [x (stk-push `(,(fn x) FP))]))
