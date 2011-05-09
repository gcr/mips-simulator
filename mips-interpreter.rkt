#lang racket
(provide all-defined-out)

(require parser-tools/lex
         parser-tools/yacc
         "opcode-table.rkt"
         "mips-machine.rkt"
         (prefix-in : parser-tools/lex-sre))

; TOKENIZING
(define-tokens value-tokens (REGISTER LITERAL LABEL SECTION STRING))
(define-empty-tokens punctuation-tokens (NEWLINE COMMA OP CP EOF))
(define-empty-tokens opcode-tokens (add addi addiu addu sub subu and andi nor or ori xor xori sll srl sra sllv srlv srav slt slti sltiu sltu beq bne blt bgt ble bge j jal jr jalr move lb lbu lh lhu lui l li la sb sh sw div divu mult multu bclt bclf))

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
   [word          (string->symbol lexeme)]
   [(:: #\. word) (token-SECTION (substring lexeme 1))]
   [#\(           (token-OP)]
   [#\)           (token-CP)]
   [#\,           (token-COMMA)]
   [(:: #\$ word) (token-REGISTER (which-register? (substring lexeme 1)))]
   [(:: word #\:) (token-LABEL (substring lexeme 0 (- (string-length lexeme) 1)))]))

(define (asm-parse! machine lexer)
  (define (load-op! name . args)
    ; convenience function for switching around args and such.
    ; for the assembler.
    (let* ([opcode (find-opcode-with-name name)]
           [binary (send (find-opcode-with-name name) make-binary . args)])
      (send machine add-opcode! binary)))  
  ((parser
    (src-pos)
    (start line-list)
    (end EOF)
    (tokens value-tokens punctuation-tokens opcode-tokens)
    (error (λ (tok-ok? tok-name tok-value start end)
             (if tok-ok?
                 (printf "Wasn't expecting \"~a\" at line ~a\n" tok-name (position-line start))
                 (printf "What on earth is a \"~a\" at line ~a?\n"
                         tok-name (position-line start)))))
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
           
           ; Here we define how each opcode is read. We may switch
           ; around arguments, do other things, etc. Mapping the
           ; semantic structure of the opcode to the actual binary.
           
           ; add rd, rs, rt
           [(add REGISTER COMMA REGISTER COMMA REGISTER NEWLINE)
            (load-op! "add" $4 $6 $2 0)]
           ;                rs rt rd shamt
           
           [(sub REGISTER COMMA REGISTER COMMA REGISTER NEWLINE)
            (load-op! "sub" $4 $6 $2 0)]
           [(addi REGISTER COMMA REGISTER COMMA LITERAL NEWLINE)
            (load-op! "addi" $4 $2 $6)]
           [(addu REGISTER COMMA REGISTER COMMA REGISTER NEWLINE)
            (load-op! "addu" $4 $6 $2 0)]
           [(addiu REGISTER COMMA REGISTER COMMA LITERAL NEWLINE)
            (load-op! "addiu" $4 $2 $6)]
           
           )))
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