#lang racket
(require test-engine/racket-tests)
(require "assembler.rkt")

(check-expect (primpl-assemble '((label LOOP-TOP)
                                 (gt TMP1 X 0)
                                 (branch TMP1 LOOP-CONT)
                                 (jump LOOP-DONE)
                                 (label LOOP-CONT)
                                 (mul Y 2 Y)
                                 (sub X X 1)
                                 (print-val Y)
                                 (print-string "\n")
                                 (jump LOOP-TOP)
                                 (label LOOP-DONE)
                                 (halt)
                                 (data X 10)
                                 (data Y 1)
                                 (data TMP1 0)))
              '((gt (11) (9) 0)
                (branch (11) 3)
                (jump 8)
                (mul (10) 2 (10))
                (sub (9) (9) 1)
                (print-val (10))
                (print-string "\n")
                (jump 0)
                0
                10
                1
                0))

(check-expect (primpl-assemble '((const A B)
                                 (label B) 
                                 (print-val A)))
              '((print-val 0)))

(check-error (primpl-assemble '((const A B)
                                (label B) 
                                (data C A B) 
                                (print-val (B C))))
             "get-operand: incorrect B")

(check-expect (primpl-assemble '((const A B)
                                 (label B) 
                                 (data C A B) 
                                 (print-val (A C))))
              '(0
                0
                (print-val (0 (0)))))

(check-expect (primpl-assemble '((const A B)
                                 (const B C)
                                 (data C 1 2 D)
                                 (const D A)
                                 (const E F)
                                 (label F)
                                 (print-val E)
                                 (data X A B C D E F)))
              '(1
                2 
                0 
                (print-val 3) 
                0 
                0 
                0 
                0 
                3 
                3))

(check-error (primpl-assemble '((const A B) 
                                (const B C) 
                                (data C 1 2 D) 
                                (const D A) 
                                (add A B C)))
             "get-destination: incorrect A")

(check-expect (primpl-assemble '((data B 1 2 B))) 
              '(1
                2
                0))

(check-error (primpl-assemble '((add (B) 1 2)
                                (const B A) 
                                (data A 1 2 3)))
             "get-operand: incorrect (B)")

(check-expect (primpl-assemble '((data B 1 2 B) 
                                 (add (B B) 2 4))) 
              '(1
                2 
                0
                (add (0 (0)) 2 4)))

(check-expect (primpl-assemble '((data B 1 2 B) 
                                 (const A B) 
                                 (lit B) 
                                 (label C) 
                                 (lit A) 
                                 (lit C)))
              '(1 
                2 
                0
                0 
                0 
                4))

(check-error (primpl-assemble '((const A B)
                                (data A 5))) 
             "add-symbol: duplicate A")

(check-error (primpl-assemble '((const A B)
                                (const B C) 
                                (const C A)))
             "check-cycle: circular A")

(check-error (primpl-assemble '((label A)
                                (add (0) A 2)))
             "get-operand: incorrect A")

(test)
