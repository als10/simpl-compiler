#lang racket
(provide primpl-assemble)

(struct label (i) #:transparent)

(define symbol-table (make-hash))

(define (add-symbol k v)
  (match (hash-ref symbol-table k #f)
    [#f (hash-set! symbol-table k v)]
    [_ (error 'add-symbol "duplicate ~a" k)]))

(define index 0)
(define (initial-parse instruction)
  (match instruction
    [`(label ,ps)
     (add-symbol ps (label index))]
    [`(const ,ps ,val)
     (add-symbol ps val)]
    [`(data ,ps (,n ,_))
     (add-symbol ps `(,index))
     (set! index (+ index n))]
    [`(data ,ps ,vals ...)
     (add-symbol ps `(,index))
     (set! index (+ index (length vals)))]
    [x (set! index (+ index 1))]))

(define (lookup psym)
  (hash-ref symbol-table psym (lambda () (error 'lookup "undefined ~a" psym))))

(define (check-cycle from)
  (define (check-cycle/h curr)
    (if (symbol? curr)
        (if (symbol=? curr from)
            (error 'check-cycle "circular ~a" curr)
            (check-cycle/h (lookup curr)))
        curr))
  (check-cycle/h (lookup from)))

(define (resolve-symbol s)
  (when (symbol? (lookup s))
    (define newv
      (match (check-cycle s)
        [(label i) i]
        [`(,x) x]
        [x x]))
    (hash-set! symbol-table s newv)
    newv))

(define (get-operand/h psym allow-label?)
  (match psym
    [`(,(? symbol? v)) (error 'get-operand "incorrect ~a" psym)]
    [`(,i ,v) `(,(get-immediate i) ,(get-indirect v))]
    [v
     (if (symbol? v)
         (match (lookup v)
           [(label i) (if allow-label? i (error 'get-operand "incorrect ~a" psym))]
           [x x])
         v)]))

(define (get-operand psym)
  (get-operand/h psym #f))

(define (get-target psym)
  (get-operand/h psym #t))

(define (get-psym-immediate psym)
  (match (get-target psym)
    [`(,_ ,_) (error 'get-psym-immediate "incorrect ~a" psym)]
    [`(,v) v]
    [v v]))

(define (get-immediate psym)
  (match (get-operand psym)
    [`(,_ ,_) (error 'get-immediate "incorrect ~a" psym)]
    [`(,v) v]
    [v v]))

(define (get-destination psym)
  (define opd (get-operand psym))
  (match opd
    [`(,i ,v) opd]
    [`(,v) opd]
    [x (error 'get-destination "incorrect ~a" psym)]))

(define (get-indirect psym)
  (define opd (get-operand psym))
  (match opd
    [`(,_) opd]
    [_ (error 'get-indirect "incorrect ~a" psym)]))

(define (main-parser instruction-list)
  (cond
    [(empty? instruction-list) empty]
    [else
     (define rorr (main-parser (rest instruction-list)))
     (match (first instruction-list)
       [`(halt)
        (cons 0 rorr)]
       [`(lit ,v)
        (cons (get-psym-immediate v) rorr)]
       [`(data ,_ (,n ,v))
        (append (build-list n (lambda (_) (get-psym-immediate v))) rorr)]
       [`(data ,_ ,v ...)
        (append (map get-psym-immediate v) rorr)]
       [`(const ,_ ,_) rorr]
       [`(label ,_) rorr]
       [`(jump ,tgt)
        (cons `(jump ,(get-target tgt)) rorr)]
       [`(jsr ,dest ,tgt)
        (cons `(jsr ,(get-destination dest) ,(get-target tgt)) rorr)]
       [`(branch ,opd ,tgt)
        (cons `(branch ,(get-operand opd) ,(get-target tgt)) rorr)]
       [`(print-val ,opd)
        (cons `(print-val ,(get-operand opd)) rorr)]
       [`(print-string ,s)
        (cons `(print-string ,(string-append "\"" s "\"")) rorr)]
       [`(,op ,dest ,opds ...)
        (cons (append `(,op ,(get-destination dest)) (map get-operand opds)) rorr)]
       [_ rorr])]))

(define (primpl-assemble instruction-list)
  (set! index 0)
  (set! symbol-table (make-hash))
  (for-each initial-parse instruction-list)
  (for-each resolve-symbol (hash-keys symbol-table))
  (main-parser instruction-list))
