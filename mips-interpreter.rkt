#lang racket
(provide all-defined-out)

(require parser-tools/lex
         parser-tools/yacc
         "opcode-table.rkt"
         "mips-machine.rkt"
         (prefix-in : parser-tools/lex-sre))

; TOKENIZING
(define-tokens value-tokens (REGISTER LITERAL WORD LABEL SECTION STRING))
(define-empty-tokens op-tokens (NEWLINE COMMA OP CP EOF))

(define-lex-abbrevs
  ;; (:/ 0 9) would not work because the lexer does not understand numbers.  (:/ #\0 #\9) is ok too.
  [digit (:/ "0" "9")]
  [number (:: (:? #\-)
              (:? (:: #\0 #\x))
              (:+ digit))]
  [word (:+ (:or alphabetic digit #\_))])

(define get-string-token
    (lexer
     [(:~ #\" #\\) (cons (car (string->list lexeme))
                         (get-string-token input-port))]
     [(:: #\\ #\\) (cons #\\ (get-string-token input-port))]
     [(:: #\\ #\") (cons #\" (get-string-token input-port))]
     [(:: #\\ #\n) (cons #\newline (get-string-token input-port))]
     [#\" '()]))

(define asm-lex
  (lexer-src-pos
   [(eof)      (token-EOF)]
   [#\newline  (token-NEWLINE)]
   [whitespace (return-without-pos (asm-lex input-port))] ; skip over whitespace
   [(:: #\#    (:* (:~ #\newline))) (return-without-pos (asm-lex input-port))] ; comments
   
   [number (token-LITERAL (string->number (regexp-replace #px"(-?)0x" lexeme "#x\\1")))]
   
   [#\" (token-STRING (list->string (get-string-token input-port)))]
   
   [word          (token-WORD lexeme)]
   [(:: #\. word) (token-SECTION (substring lexeme 1))]
   [#\(           (token-OP)]
   [#\)           (token-CP)]
   [#\,           (token-COMMA)]
   [(:: #\$ word) (token-REGISTER (substring lexeme 1))]
   [(:: word #\:) (token-LABEL (substring lexeme 0 (- (string-length lexeme) 1)))]))

(define (asm-parse! machine lexer)
  (define (op name . args)
    ; convenience function for switching around args and such.
    ; for the assembler.
    (let* ([opcode (find-opcode-with-name name)]
           [args (match (cons name args)
                   ; tweaks to each opcode.
                   ; This transforms what's written (e.g. "add $rd, $rs, $rt")
                   ; into the actual binary in the opcode (e.g. rs, rt, rd, shamt)
                   [(list "add" rd rs rt)   (list rs rt rd 0)]
                   [(list "addi" rt rs imm) (list rs rt imm)]
                   [(list "addu" rd rs rt)   (list rs rt rd 0)]
                   [(list "addiu" rt rs imm) (list rs rt imm)]
                   [(list "sub" rd rs rt) (list rs rt rd 0)]
                   [(list "subu" rd rs rt) (list rs rt rd 0)]
                   [(cons _ args) args])] ; else
           [binary (send (find-opcode-with-name name) make-binary . args)])
      (send machine add-opcode! binary)))
  
  ((parser
    (src-pos)
    (start line-list)
    (end EOF)
    (tokens value-tokens op-tokens)
    (error (λ (tok-ok? tok-name tok-value start end)
             (if tok-ok?
                 (printf "Wasn't expecting a ~a at line ~a\n" tok-name (position-line start))
                 (printf "What on earth is this? ~a ~a at line ~a\n"
                         tok-name tok-value (position-line start)))))
    (grammar
     (line-list [() '()]
                [(line line-list) (cons $1 $2)]
                [(NEWLINE line-list) $2])
     ; most of these are passing the list verbatim to 'op' above which handles all the dirty work
     (line [(LABEL)
            `(label ,$1)]
           [(SECTION NEWLINE)
            `(section ,$1)]
           [(SECTION STRING NEWLINE)
            `(declaration-str ,$1 ,$2)]
           [(SECTION LITERAL NEWLINE)
            `(declaration-literal ,$1 ,$2)]
           [(WORD place COMMA place COMMA place NEWLINE)
            (op $1 $2 $4 $6)] ; arithmetic and immediate
           [(WORD WORD NEWLINE)
            (op $1 $2)]
           [(WORD REGISTER NEWLINE)
            (op $1 $2)]
           [(WORD NEWLINE)
            (op $1)]
           [(WORD place COMMA WORD NEWLINE)
            (op $1 $2 $4)]
           [(WORD place COMMA place NEWLINE)
            (op $1 $2 $4)])
     
     (place [(REGISTER) (which-register? $1)]
            [(LITERAL) $1]
            [(LITERAL OP REGISTER CP) `(offset ,$3 ,$1)])))
   lexer))

(define (asm-load-into-machine input)
  (port-count-lines! input)
  (define m (new mips-machine%))
  (asm-parse! m (λ () (asm-lex input)))
  (send m set-pc! 0)
  m)

;;;
(define f (open-input-string (string-join
                              '("addi $t0, $zero, 125"
                                "add $t1, $t0, $t0"
                                "sub $t2, $t0, $t1"
                                ""
                                ) "\n")))

(define m (asm-load-into-machine f))