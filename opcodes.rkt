#lang racket
(provide (all-defined-out))

(require racket/match)

(define (binary-matches? binary matchers)
  ;; Returns #t if the given binary matches all the matchers
  ;; each matcher is '(binary-target nbits) which means "the next
  ;; nbits must match binary-target". Matchers can be simple
  ;; numbers too, which means just skip that many and match anything.
  (let next-matcher ([current-bit-pos 0]
                     [matchers (reverse matchers)]
                     [wildcard-bit-patterns '()])
    (match matchers
      [(cons (list target n-bits) rest)
       (if (= (bitwise-bit-field binary current-bit-pos (+ current-bit-pos n-bits)) target)
           (next-matcher (+ current-bit-pos n-bits) rest wildcard-bit-patterns)
           #f)]
      [(cons n-skip-bits rest)
       (next-matcher (+ current-bit-pos n-skip-bits) rest
                     (cons (bitwise-bit-field binary current-bit-pos (+ current-bit-pos n-skip-bits))
                           wildcard-bit-patterns))]
      ['() wildcard-bit-patterns])))

(define opcode%
  (class object%
    (init-field name matches funct)
    (super-new)
    
    (define/public (matches? binary)
      ; returns the holes if this opcode matches the given binary
      (binary-matches? binary matches))
    
    (define/public (with-args binary funct)
      ; calls funct with the skipped values in the binary as determined by the matchers
      (let ([args (matches? binary)])
        (apply funct args)))
    
    (define/public (execute binary . extra-args)
      ; runs the given binary opcode, optionally with extra arguments...
      (with-args binary (λ args
                          (printf "executing ~a with ~a and ~a" name args extra-args)
                          (apply funct (append args extra-args)))))
    
    (define/public (make-binary . args)
      (printf "making binary ~a w/ args ~a\n" name args)
      ; returns a new binary number w/ the holes
      (let next-matcher ([sofar 0]
                         [matchers matches]
                         [args args])
        (match matchers
          [(cons (list target n-bits) rest-matchers)
           (next-matcher (+ (arithmetic-shift sofar n-bits)
                            (bitwise-bit-field target 0 n-bits))
                         rest-matchers
                         args)]
          [(cons n-bits rest-matchers)
           (next-matcher (+ (arithmetic-shift sofar n-bits)
                            (bitwise-bit-field (car args) 0 n-bits))
                         rest-matchers
                         (cdr args))]
          ['() sofar])))
    
    ))
  
  #|
;(define x (new opcode% [name "hello"] [matches '((#xff 8) 4 (#x23 8) 8)] [funct (λ ())]))
(define t (new opcode% [name "test"] [matches '((#xff 8) 8 (#xee 8))] [funct void]))
(send t matches? #xff11ee)
(send t matches? #xfe01ee)
(send t with-args #xffffee (λ (x) (printf "got number ~a\n" x)))
  |#