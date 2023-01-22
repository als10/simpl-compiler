#lang racket
(require "arithmetic-expressions.rkt" "utils.rkt")
(provide compile-bexp)

;; converts a given SIMPL boolean expression [be] into a list of A-PRIMPL
;; instructions
;; [fn] is a function that takes in a string and produces a symbol with
;; the given string added to the end of the function name
(define (compile-bexp be fn)
  (match be
    [`(,op ,opds ...)
     (cond
       [(member op '(and or not))
        (define primpl-op (op->primpl-op op))
        (define inst-opds
          (foldl (lambda (_ acc)
                   (match acc
                     ['() `((-1 SP))]
                     [`((,f ,_) ,_ ...) (cons `(,(- f 1) SP) acc)]))
                 empty
                 opds))
        (define first-opd
          (match inst-opds
            [`((,f ,_) ,_ ...) f]))
        (append (compile-seq compile-bexp opds fn)
                `(,(append `(,primpl-op (,first-opd SP)) inst-opds)
                   (sub SP SP ,(- (* -1 first-opd) 1))))]
       [else (compile-aexp be fn)])]
    ['true (stk-push #t)]
    ['false (stk-push #f)]
    [x (stk-push `(,(fn x) FP))]))
