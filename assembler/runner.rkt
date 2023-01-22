#lang racket
(require "assembler.rkt")

(define (main)
  (define aprimp (read))
  (define primp (primpl-assemble aprimp))
  (for-each (lambda (instruction)
              (display instruction)
              (newline))
            primp))

(main)
