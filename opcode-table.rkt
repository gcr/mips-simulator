#lang racket
(provide (all-defined-out))

(require "opcodes.rkt")

; convenience functions
(define (get-reg mips reg) (send mips get-register reg))
(define (set-reg! mips reg val) (send mips set-register! reg val))

(define (sign-extend n)
  ;; sign-extends n from 16 bits to 32 bits
  (let ([leftmost-bit (if (bitwise-bit-set? n 15) 1 0)])
    (+ (arithmetic-shift (bitwise-bit-field (- leftmost-bit) 0 16)
                         16)
       (bitwise-bit-field n 0 16))))
  
(define opcode-table
  (list
   (new opcode%
        [name "add"]
        [matches '([#x0 6] 5 5 5 5 [#x20 6])]
        [funct (λ (rs rt rd shamt mips)
                 (set-reg! mips rd (+ (get-reg mips rs) (get-reg mips rt))))])
   
   (new opcode%
        [name "addi"]
        [matches '([#x8 6] 5 5 16)]
        [funct (λ (rs rt imm mips)
                 (set-reg! mips rt (+ (get-reg mips rs) (sign-extend imm))))])
   
   (new opcode%
        [name "addiu"]
        [matches '([#x9 6] 5 5 16)]
        [funct (λ (rs rt imm mips)
                 (set-reg! mips rt (+ (get-reg mips rs) (sign-extend imm))))])
   
   (new opcode%
        [name "addu"]
        [matches '([#x0 6] 5 5 5 5 [#x21 6])]
        [funct (λ (rs rt rd shamt mips)
                 (set-reg! mips rd (+ (get-reg mips rs) (get-reg mips rt))))])
   
   (new opcode%
        [name "sub"]
        [matches '([#x0 6] 5 5 5 5 [#x22 6])]
        [funct (λ (rs rt rd shamt mips)
                 (set-reg! mips rd (- (get-reg mips rs) (get-reg mips rt))))])
   
   (new opcode%
        [name "subu"]
        [matches '([#x0 6] 5 5 5 5 [#x23 6])]
        [funct (λ (rs rt rd shamt mips)
                 (set-reg! mips rd (- (get-reg mips rs) (get-reg mips rt))))])
   
   ))

(define (find-opcode-with-name name)
  (findf (λ (op)
           (equal? (get-field name op)
                   name))
         opcode-table))

(define (find-matching-opcode binary)
  (findf (λ (op) (send op matches? binary)) opcode-table))

#|
(require "mips-machine.rkt")

(define m (new mips-machine%))
(define adder (cadr opcode-table))
(define a1 (send adder make-binary 0 1 1250))
(format "~b" a1)
(send adder execute a1 m)
(send adder execute (send adder make-binary 1 2 1) m)
(send adder execute (send adder make-binary 0 3 -1) m)
(send adder execute (send adder make-binary 0 4 32767) m)
(send adder execute (send adder make-binary 0 5 32768) m)
(get-field registers m)
|#