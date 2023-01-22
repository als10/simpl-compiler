#lang racket
(require "compiler.rkt")

(define (main)
  (define simpl (read))
  (define aprimp (compile-simpl simpl))
  (for-each (lambda (instruction)
              (display instruction)
              (newline))
            aprimp))

(main)
