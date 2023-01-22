#lang racket
(require racket/syntax)
(provide reset-funcs 
         get-fn 
         add-fn
         extend-symbol
         append-fn
         op->primpl-op
         reset-label-counter
         generate-label
         stk-push
         compile-seq)

(define funcs (make-hash))

(define (reset-funcs)
  (set! funcs (make-hash)))

(define (get-fn fname)
  (hash-ref funcs fname #f))

;; adds [fname] and the number of arguments in the function
;; to [funcs]
(define (add-fn fname args)
  (match (hash-ref funcs fname #f)
    [#f (hash-set! funcs fname (length args))]
    [_ (error "duplicate function")]))

(define label-counter 0)

(define (reset-label-counter)
  (set! label-counter 0))

;; generates a new label and increments the label counter
(define (generate-label)
  (set! label-counter (+ 1 label-counter))
  (format-symbol "LABEL~a" label-counter))

;; extends the symbol [start] with [end], with '_' in between
(define (extend-symbol start end)
  (format-symbol "~a_~a" start end))

;; creates a label for a function by appending the function name [fname]
;; to FN_
(define (append-fn fname)
  (format-symbol "FN_~a" fname))

(define (op->primpl-op op)
  (format-symbol "l~a" op))

;; produces a list of A-PRIMPL instructions that pushes [v] onto the stack
(define (stk-push v)
  `((move (0 SP) ,v) 
    (add SP SP 1)))

;; applies the [compile-fn] to a sequence of SIMPL statements [stmts]
;; [fn] is a function that takes in a string and produces a symbol with
;; the given string added to the end of the function name
(define (compile-seq compiler stmts fn)
  (apply append (map (lambda (s) (compiler s fn)) stmts)))
