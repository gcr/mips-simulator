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

(define (signed n)
  ;; usually registers are 32-bit unsigned.
  ;; this converts them into 32-bit signed.
  ;; (dirty hack, teehee!)
  (integer-bytes->integer (integer->integer-bytes n 4 #f #t)
                          #t #t))
  
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
   
   (new opcode%
        [name "and"]
        [matches '([#x0 6] 5 5 5 5 [#x24 6])]
        [funct (λ (rs rt rd shamt mips)
                 (set-reg! mips rd (bitwise-and (get-reg mips rs) (get-reg mips rt))))])
   
   (new opcode%
        [name "andi"]
        [matches '([#xc 6] 5 5 16)]
        [funct (λ (rs rt imm mips)
                 (set-reg! mips rt (bitwise-and (get-reg mips rs) imm)))])
   
   (new opcode%
        [name "nor"]
        [matches '([#x0 6] 5 5 5 5 [#x27 6])]
        [funct (λ (rs rt rd shamt mips)
                 (set-reg! mips rd (bitwise-not (bitwise-ior (get-reg mips rs) (get-reg mips rt)))))])
   
   (new opcode%
        [name "or"]
        [matches '([#x0 6] 5 5 5 5 [#x25 6])]
        [funct (λ (rs rt rd shamt mips)
                 (set-reg! mips rd (bitwise-ior (get-reg mips rs) (get-reg mips rt))))])
   
   (new opcode%
        [name "ori"]
        [matches '([#xd 6] 5 5 16)]
        [funct (λ (rs rt imm mips)
                 (set-reg! mips rt (bitwise-ior (get-reg mips rs) imm)))])

   (new opcode%
        [name "xor"]
        [matches '([#x0 6] 5 5 5 5 [#x26 6])]
        [funct (λ (rs rt rd shamt mips)
                 (set-reg! mips rd (bitwise-xor (get-reg mips rs) (get-reg mips rt))))])
   
   (new opcode%
        [name "xori"]
        [matches '([#xe 6] 5 5 16)]
        [funct (λ (rs rt imm mips)
                 (set-reg! mips rt (bitwise-xor (get-reg mips rs) imm)))])
   
   (new opcode%
        [name "sll"]
        [matches '([#x0 6] 5 5 5 5 [#x0 6])]
        [funct (λ (rs rt rd shamt mips)
                 (set-reg! mips rd (arithmetic-shift (get-reg mips rt) shamt)))])
   
   (new opcode%
        [name "srl"]
        [matches '([#x0 6] 5 5 5 5 [#x2 6])]
        [funct (λ (rs rt rd shamt mips)
                 (set-reg! mips rd (arithmetic-shift (get-reg mips rt) (- shamt))))])
   
   (new opcode%
        [name "sra"]
        [matches '([#x0 6] 5 5 5 5 [#x3 6])]
        [funct (λ (rs rt rd shamt mips)
                 (set-reg! mips rd (arithmetic-shift (signed (get-reg mips rt)) (- shamt))))])
   
   (new opcode%
        [name "sllv"]
        [matches '([#x0 6] 5 5 5 5 [#x4 6])]
        [funct (λ (rs rt rd shamt mips)
                 (set-reg! mips rd (arithmetic-shift (get-reg mips rt) (get-reg mips rs))))])
   
   (new opcode%
        [name "srlv"]
        [matches '([#x0 6] 5 5 5 5 [#x6 6])]
        [funct (λ (rs rt rd shamt mips)
                 (set-reg! mips rd (arithmetic-shift (get-reg mips rt) (- (get-reg mips rs)))))])
   
   (new opcode%
        [name "srav"]
        [matches '([#x0 6] 5 5 5 5 [#x7 6])]
        [funct (λ (rs rt rd shamt mips)
                 (set-reg! mips rd (arithmetic-shift (signed (get-reg mips rt)) (- (get-reg mips rs)))))])
   
   (new opcode%
        [name "slt"]
        [matches '([#x0 6] 5 5 5 5 [#x2a 6])]
        [funct (λ (rs rt rd shamt mips)
                 (set-reg! mips rd (if (< (signed (get-reg mips rs))
                                          (signed (get-reg mips rt)))
                                       1 0)))])
   
   (new opcode%
        [name "slti"]
        [matches '([#xa 6] 5 5 16)]
        [funct (λ (rs rt imm mips)
                 (set-reg! mips rt (if (< (signed (get-reg mips rs))
                                          (signed (sign-extend imm)))
                                       1 0)))])
   
   (new opcode%
        [name "sltu"]
        [matches '([#x0 6] 5 5 5 5 [#x2b 6])]
        [funct (λ (rs rt rd shamt mips)
                 (set-reg! mips rd (if (< (get-reg mips rs) (get-reg mips rt))
                                       1 0)))])
   
   (new opcode%
        [name "sltiu"]
        [matches '([#xb 6] 5 5 16)]
        [funct (λ (rs rt imm mips)
                 (set-reg! mips rt (if (< (get-reg mips rs) imm) 1 0)))])
   
   (new opcode%
        [name "beq"]
        [matches '([#x4 6] 5 5 16)]
        [funct (λ (rs rt addr mips)
                 (let ([addr (signed (sign-extend addr))])
                   (when (= (get-reg mips rs) (get-reg mips rt))
                     (send mips set-pc! (+ (send mips get-pc) (* addr 4))))))])
   
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