#lang racket
(provide (all-defined-out))
(require "opcode-table.rkt")

(define (which-register? reg-string)
  (match reg-string
    ["zero" 0]
    ["at" 1]
    ["gp" 28]
    ["sp" 29]
    ["fp" 30]
    ["ra" 31]
    [(regexp #rx"v(.)" (list _ n)) (+ 2 (string->number n))]
    [(regexp #rx"a(.)" (list _ n)) (+ 4 (string->number n))]
    [(regexp #rx"t(.)" (list _ n))
     (let ([n (string->number n)])
       (if (> n 7) (+ 24 (- n 8)) (+ 8 n)))]
    [(regexp #rx"s(.)" (list _ n)) (+ 16 (string->number n))]
    [(regexp #rx"k(.)" (list _ n)) (+ 26 (string->number n))]
    [n (string->number n)]))

(define mips-machine%
  (class object%
    (super-new)
    
    (field [registers (make-vector 32)]
           [memory (make-bytes 32)]
           [pc 0])
    
    ; Registers
    (define/public (get-register reg)
      (bitwise-bit-field (vector-ref registers
                                     (if (number? reg) reg (which-register? reg)))
                         0 32))
    (define/public (set-register! reg val)
      (vector-set! registers
                   (if (number? reg) reg (which-register? reg))
                   (bitwise-bit-field val 0 32)))
    
    ; Bitewise memory ops
    (define/public (mem-byte addr)
      (bytes-ref memory addr))
    (define/public (mem-set-byte! addr val)
      (bytes-set! memory addr val))
    
    ; Wordwise memory ops (UNALIGNED!)
    (define/public (mem-word addr signed?)
      (integer-bytes->integer memory signed? #t addr (+ 4 addr)))
    (define/public (mem-set-word! addr val signed?)
      (integer->integer-bytes val 4 signed? #t memory addr))
    
    (define/public (add-opcode! opcode)
      (mem-set-word! pc opcode #f)
      (set! pc (+ 4 pc)))
    
    (define/public (add-opcode-to! addr opcode)
      (mem-set-word! addr opcode #f))
    
    (define/public (set-pc! val)
      (set! pc val))
    (define/public (get-pc)
      pc)
    
    (define/public (debug-registers)
      (for ([i (in-vector registers)]
            [j (in-naturals)])
        (printf "~a: ~b\n" j i)))
    
    (define/public (step-one!)
      (let* ([next-instruction (mem-word pc #f)]
             [opcode (find-matching-opcode next-instruction)])
        (if opcode
            (begin (printf "step: ~a ~a\n"
                           (get-field name opcode)
                           (send opcode matches? next-instruction))
                   (set! pc (+ 4 pc))
                   (send opcode execute next-instruction this))
            (error 'step-one! "Unknown opcode at address ~a: ~b" pc next-instruction))))
    
    ))