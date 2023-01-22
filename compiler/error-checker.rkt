#lang racket
(provide error-checker)

;; checks the [lst] for duplicate elements
(define (check-duplicates lst)
  (unless (empty? lst)
    (if (member (first lst) (rest lst)) 
        (error "duplicate") 
        (check-duplicates (rest lst)))))

;; checks the function body [stmts] for a possible return error, ie,
;; whether the last statement is a return statement
(define (check-return stmts)
  (if (empty? (rest stmts))
      (unless (symbol=? 'return (first (first stmts)))
        (error "return"))
      (check-return (rest stmts))))

(define (error-checker vars-list args stmts)
  (check-duplicates vars-list)
  (check-duplicates args)
  (check-return stmts))
