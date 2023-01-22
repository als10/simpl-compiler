#lang racket
(require "functions.rkt" "utils.rkt")
(provide compile-simpl)

;; converts the given list of SIMPL-F functions [prog]
;; into a list of A-PRIMPL instructions
(define (compile-simpl prog)
  (reset-label-counter)
  (reset-funcs)
  (for-each (lambda (f)
              (match f
                [`(fun (,fname ,args ...) ,_) (add-fn fname args)]))
            prog)
  (append (match (get-fn 'main)
            [#f `((halt))]
            [_ `((jump FN_main))])
          (apply append (map compile-fn prog))
          `((data RETURN-VAL 0) 
            (data RETURN-ADDR END) 
            (data FP 0) 
            (data SP END) 
            (label END))))
