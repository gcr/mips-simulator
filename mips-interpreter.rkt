#lang racket
(provide all-defined-out)

(require parser-tools/lex
         parser-tools/yacc
         "opcode-table.rkt"
         "mips-machine.rkt"
         (prefix-in : parser-tools/lex-sre))

; TOKENIZING
(define-tokens value-tokens (REGISTER LITERAL LABEL WORD SECTION STRING))
(define-empty-tokens punctuation-tokens (NEWLINE COMMA OP CP EOF))
(define-empty-tokens opcode-tokens (add addi addiu addu sub subu and andi nor or ori xor xori sll srl sra sllv srlv srav slt slti sltiu sltu beq bne blt bgt ble bge j jal jr jalr move lb lbu lh lhu lui lw li la sb sh sw div divu mult multu bclt bclf syscall))

(define-lex-abbrevs
  ;; (:/ 0 9) would not work because the lexer does not understand numbers.  (:/ #\0 #\9) is ok too.
  [digit (:/ "0" "9")]
  [number (:: (:? #\-)
              (:? (:: #\0 #\x))
              (:+ digit))]
  [opcode (:or "add" "addi" "addiu" "addu" "sub" "subu" "and" "andi" "nor" "or" "ori" "xor" "xori" "sll" "srl" "sra" "sllv" "srlv" "srav" "slt" "slti" "sltiu" "sltu" "beq" "bne" "blt" "bgt" "ble" "bge" "j" "jal" "jr" "jalr" "move" "lb" "lbu" "lh" "lhu" "lui" "lw" "li" "la" "sb" "sh" "sw" "div" "divu" "mult" "multu" "bclt" "bclf" "syscall")]
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
   [opcode         (string->symbol lexeme)]
   [word           (token-WORD lexeme)]  
   [(:: #\. word) (token-SECTION (substring lexeme 1))]
   [#\(           (token-OP)]
   [#\)           (token-CP)]
   [#\,           (token-COMMA)]
   [(:: #\$ word) (token-REGISTER (which-register? (substring lexeme 1)))]
   [(:: word #\:) (token-LABEL (substring lexeme 0 (- (string-length lexeme) 1)))]))

(define (asm-parse! machine lexer)
  (define post-compile-operations '())
  (define label-table (make-hash))
  
  (define (load-op! name . args)
    ; convenience function: load the given op into the current machine
    (let* ([opcode (find-opcode-with-name name)]
           [binary (send (find-opcode-with-name name) make-binary . args)])
      (printf "~a ~a\n" name args)
      (send machine add-opcode! binary)))
  (define (load-op-to! addr name . args)
    (let* ([opcode (find-opcode-with-name name)]
           [binary (send (find-opcode-with-name name) make-binary . args)])
      (send machine add-opcode-to! addr binary)))
  
  (define (defer-op! name . args)
    ; set up this op to be filled in with a label when the time comes
    (let ([current-addr (send machine get-pc)])
      (printf "Deferred op at addr ~a: ~a ~a\n" current-addr name args)
      (set! post-compile-operations
            (cons (λ ()
                    (printf "Applying ~a ~a to address ~a\n" name args current-addr)
                    ; when it's all compiled, fill it in true
                    (apply load-op-to! current-addr name
                           (map (match-lambda [(box label) (/ (- (hash-ref label-table label) current-addr 4) 4)]
                                              [x x]) args)))
                  post-compile-operations))
      ; for now, pretend it's a 0
      (apply load-op! name
             (map (match-lambda [(box label) 0]
                                [x x]) args))))
  
  (define (new-label! word)
    ; sets this label in the label table
    (printf "Label: ~a at addr ~a\n" word (send machine get-pc))
    (hash-set! label-table word (send machine get-pc)))
  
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
     ; most of these are passing the list to 'load-op!' above which handles all the dirty work
     (line [(LABEL)
            (new-label! $1)] ; TODO
           [(SECTION NEWLINE)
            `(section ,$1)] ; TODO
           [(SECTION STRING NEWLINE)
            `(declaration-str ,$1 ,$2)] ; TODO
           [(SECTION LITERAL NEWLINE)
            `(declaration-literal ,$1 ,$2)] ; TODO
           
           ; Here we define how each opcode is read. We may switch
           ; around arguments, do other things, etc. Mapping the
           ; semantic structure of the opcode to the actual binary.
           
           ; add rd, rs, rt
           [(add REGISTER COMMA REGISTER COMMA REGISTER NEWLINE) ; how it's read
            (load-op! "add" $4 $6 $2 0)]                         ; how it's encoded
           ;                rs rt rd shamt
           
           [(sub REGISTER COMMA REGISTER COMMA REGISTER NEWLINE)
            (load-op! "sub"  $4 $6 $2 0)]
           [(addi REGISTER COMMA REGISTER COMMA LITERAL NEWLINE)
            (load-op! "addi"  $4 $2 $6)]
           [(addu REGISTER COMMA REGISTER COMMA REGISTER NEWLINE)
            (load-op! "addu"  $4 $6 $2 0)]
           [(addiu REGISTER COMMA REGISTER COMMA LITERAL NEWLINE)
            (load-op! "addiu"  $4 $2 $6)]
           [(and REGISTER COMMA REGISTER COMMA REGISTER NEWLINE)
            (load-op! "and"  $4 $6 $2 0)]
           [(andi REGISTER COMMA REGISTER COMMA LITERAL NEWLINE)
            (load-op! "andi"  $4 $2 $6)]
           [(nor REGISTER COMMA REGISTER COMMA REGISTER NEWLINE)
            (load-op! "nor"  $4 $6 $2 0)]
           [(or REGISTER COMMA REGISTER COMMA REGISTER NEWLINE)
            (load-op! "or"  $4 $6 $2 0)]
           [(ori REGISTER COMMA REGISTER COMMA LITERAL NEWLINE)
            (load-op! "ori"  $4 $2 $6)]
           [(xor REGISTER COMMA REGISTER COMMA REGISTER NEWLINE)
            (load-op! "xor"  $4 $6 $2 0)]
           [(xori REGISTER COMMA REGISTER COMMA LITERAL NEWLINE)
            (load-op! "xori"  $4 $2 $6)]
           [(sll REGISTER COMMA REGISTER COMMA LITERAL NEWLINE)
            (load-op! "sll"  0 $4 $2 $6)]
           [(srl REGISTER COMMA REGISTER COMMA LITERAL NEWLINE)
            (load-op! "srl"  0 $4 $2 $6)]
           [(sra REGISTER COMMA REGISTER COMMA LITERAL NEWLINE)
            (load-op! "sra"  0 $4 $2 $6)]
           [(sllv REGISTER COMMA REGISTER COMMA REGISTER NEWLINE)
            (load-op! "sllv"  $2 $4 $6 0)]
           [(srlv REGISTER COMMA REGISTER COMMA REGISTER NEWLINE)
            (load-op! "srlv"  $2 $4 $6 0)]
           [(srav REGISTER COMMA REGISTER COMMA REGISTER NEWLINE)
            (load-op! "srav"  $2 $4 $6 0)]
           [(slt REGISTER COMMA REGISTER COMMA REGISTER NEWLINE)
            (load-op! "slt"  $4 $6 $2 0)]
           [(slti REGISTER COMMA REGISTER COMMA LITERAL NEWLINE)
            (load-op! "slti"  $4 $2 $6)]
           [(sltiu REGISTER COMMA REGISTER COMMA LITERAL NEWLINE)
            (load-op! "sltiu"  $4 $2 $6)]
           [(sltu REGISTER COMMA REGISTER COMMA REGISTER NEWLINE)
            (load-op! "sltu"  $4 $6 $2 0)]
           [(beq REGISTER COMMA REGISTER COMMA WORD NEWLINE)
            (defer-op! "beq"  $2 $4 (box $6))]
           [(bne REGISTER COMMA REGISTER COMMA WORD NEWLINE)
            (defer-op! "bne"  $2 $4 (box $6))]
           [(blt REGISTER COMMA REGISTER COMMA WORD NEWLINE) ; TODO TEST!!!
            (begin (load-op! "slt"  $2 $4 (which-register? "at"))
                   (defer-op! "bne"  (which-register? "at") (which-register? ("zero") (box $6))))]
           ; TODO: bgt, ble, bge
           [(j WORD NEWLINE)
            (defer-op! "j" (box $2))]
           [(jal WORD NEWLINE)
            (defer-op! "jal" (box $2))]
           [(jr REGISTER NEWLINE)
            (load-op! "jr" $2 0 0 0)]
           [(jalr REGISTER NEWLINE)
            (load-op! "jalr" $2 0 0 0)]
           ; TODO: move
           [(syscall NEWLINE)
            (load-op! "syscall")]
           [(lb REGISTER COMMA LITERAL OP REGISTER CP NEWLINE)
            (load-op! "lb" $6 $2 $4)]
           [(lbu REGISTER COMMA LITERAL OP REGISTER CP NEWLINE)
            (load-op! "lbu" $6 $2 $4)]
           [(lh REGISTER COMMA LITERAL OP REGISTER CP NEWLINE)
            (load-op! "lh" $6 $2 $4)]
           [(lhu REGISTER COMMA LITERAL OP REGISTER CP NEWLINE)
            (load-op! "lhu" $6 $2 $4)]
           [(lui REGISTER COMMA LITERAL NEWLINE)
            (load-op! "lui" 0 $2 $4)]
           [(lw REGISTER COMMA LITERAL OP REGISTER CP NEWLINE)
            (load-op! "lw" $6 $2 $4)]
           ; TODO: li, la
           [(sb REGISTER COMMA LITERAL OP REGISTER CP NEWLINE)
            (load-op! "sb" $6 $2 $4)]
           [(sh REGISTER COMMA LITERAL OP REGISTER CP NEWLINE)
            (load-op! "sh" $6 $2 $4)]
           [(sw REGISTER COMMA LITERAL OP REGISTER CP NEWLINE)
            (load-op! "sw" $6 $2 $4)]
           
           
           
           )))
   lexer)
  (when (> (length post-compile-operations) 0)
    (displayln "Second pass...")
    (map (λ (op) (op)) post-compile-operations)))

(define (asm-load-into-machine input)
  (port-count-lines! input)
  (define m (new mips-machine%))
  (asm-parse! m (λ () (asm-lex input)))
  (send m set-pc! 0)
  m)

;;;
(define f (open-input-string #<<EOF
     addi $t0, $zero, 12
bar: addi $t1, $t1, 5
     beq $zero, $zero, foo
     sub $t2, $t2, $t2
     sub $t2, $t2, $t2
foo: beq $zero, $zero, bar

EOF
                             ))

(define m (asm-load-into-machine f))