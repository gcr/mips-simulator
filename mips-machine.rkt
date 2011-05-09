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
           [memory (make-bytes 8192)]
           [pc 4096])
    
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
      (unless (= (modulo addr 4) 0)
          (error 'add-opcode! "tried to access memory at address 0x~x which isn't word-aligned" pc))
      (integer-bytes->integer memory signed? #t addr (+ 4 addr)))
    (define/public (mem-set-word! addr val signed?)
      (unless (= (modulo addr 4) 0)
          (error 'mem-set-word! "tried to change memory at address 0x~x which isn't word-aligned" pc))
      (integer->integer-bytes val 4 signed? #t memory addr))
    
    (define/public (add-opcode! opcode)
      (unless (= (modulo pc 4) 0)
          (error 'add-opcode! "tried to change memory at address 0x~x which isn't word-aligned" pc))
      (mem-set-word! pc opcode #f)
      (set! pc (+ 4 pc)))
    
    (define/public (add-opcode-to! addr opcode)
      (unless (= (modulo addr 4) 0)
          (error 'add-opcode-to! "tried to change memory at address 0x~x which isn't word-aligned" pc))
      (mem-set-word! addr opcode #f))
    
    (define/public (set-pc! val)
      (set! pc val))
    (define/public (get-pc)
      pc)
    
    (define/public (reset-pc-text!)
      (set! pc 4096))
    (define/public (reset-pc-data!)
      (set! pc 0))
    
    (define/public (add-bytes-at! addr b)
      ;(unless (= (modulo addr 4) 0)
      ;    (error 'add-bytes! "tried to change memory at address 0x~x which isn't word-aligned" pc))
      (bytes-copy! memory addr b))
    (define/public (add-bytes! b)
      (add-bytes-at! pc b)
      (set! pc (+ pc (bytes-length b))))
    
    (define/public (align! power-of-two)
      (let ([align (expt 2 power-of-two)])
        (set! pc (+ pc (modulo (- pc) align)))))
    
    (define/public (debug-registers)
      (for ([i (in-vector registers)]
            [j (in-naturals)])
        (printf "~a: ~b\n" j i)))
    
    (define/public (step-one! [trace? #t])
      (let* ([next-instruction (mem-word pc #f)]
             [opcode (find-matching-opcode next-instruction)])
        (if opcode
            (begin (when trace?
                     (printf "step: ~a ~a\n"
                             (get-field name opcode)
                             (send opcode matches? next-instruction)))
                   (set! pc (+ 4 pc))
                   (send opcode execute next-instruction this))
            (error 'step-one! "Unknown opcode at address ~a: ~b" pc next-instruction))))
    
    (define/public (run! [trace? #f])
      (let iter ()
        (step-one! trace?)
        (iter)))
    
    ))