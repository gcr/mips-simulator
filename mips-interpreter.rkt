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
(define-empty-tokens opcode-tokens (add addi addiu addu sub subu and andi nor or ori xor xori sll srl sra sllv srlv srav slt slti sltiu sltu beq bne blt bgt ble bge j jal jr jalr move lb lbu lh lhu lui lw li la sb sh sw div divu mult multu bclt bclf syscall .text .data .asciiz .ascii .align .space))

(define-lex-abbrevs
  ;; (:/ 0 9) would not work because the lexer does not understand numbers.  (:/ #\0 #\9) is ok too.
  [digit (:/ "0" "9")]
  [number (:: (:? #\-)
              (:? (:: #\0 #\x))
              (:+ digit))]
  [opcode (:or "add" "addi" "addiu" "addu" "sub" "subu" "and" "andi" "nor" "or" "ori" "xor" "xori" "sll" "srl" "sra" "sllv" "srlv" "srav" "slt" "slti" "sltiu" "sltu" "beq" "bne" "blt" "bgt" "ble" "bge" "j" "jal" "jr" "jalr" "move" "lb" "lbu" "lh" "lhu" "lui" "lw" "li" "la" "sb" "sh" "sw" "div" "divu" "mult" "multu" "bclt" "bclf" "syscall" ".text" ".data" ".asciiz" ".ascii" ".align" ".space")]
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
                           (map (match-lambda [`(branch ,label)
                                               (/ (- (hash-ref label-table label) current-addr 4) 4)]
                                              [`(jump ,label)
                                               (/ (hash-ref label-table label) 4)]
                                              [`(la-high ,label)
                                               (bitwise-bit-field (hash-ref label-table label) 16 32)]
                                              [`(la-low ,label)
                                               (bitwise-bit-field (hash-ref label-table label) 0 16)]
                                              [x x]) args)))
                  post-compile-operations))
      ; for now, pretend it's a 0
      (apply load-op! name
             (map (match-lambda [(list _ label) 0]
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
            (new-label! $1)]
           [(.text)
            (send machine reset-pc-text!)]
           [(.data)
            (send machine reset-pc-data!)]
           [(.asciiz STRING)
            (send machine add-bytes! (bytes-append (string->bytes/utf-8 $2) #"\0"))]
           [(.ascii STRING)
            (send machine add-bytes! (string->bytes/utf-8 $2))]
           [(.space LITERAL)
            (send machine add-bytes! (make-bytes $2))]
           [(.align LITERAL)
            (send machine align! $2)]
           [(SECTION LITERAL)
            `(declaration-literal ,$1 ,$2)] ; TODO
           
           ; Here we define how each opcode is read. We may switch
           ; around arguments, do other things, etc. Mapping the
           ; semantic structure of the opcode to the actual binary.
           
           ; add rd, rs, rt
           [(add REGISTER COMMA REGISTER COMMA REGISTER) ; how it's read
            (load-op! "add" $4 $6 $2 0)]                         ; how it's encoded
           ;                rs rt rd shamt
           
           [(sub REGISTER COMMA REGISTER COMMA REGISTER)
            (load-op! "sub"  $4 $6 $2 0)]
           [(addi REGISTER COMMA REGISTER COMMA LITERAL)
            (load-op! "addi"  $4 $2 $6)]
           [(addu REGISTER COMMA REGISTER COMMA REGISTER)
            (load-op! "addu"  $4 $6 $2 0)]
           [(addiu REGISTER COMMA REGISTER COMMA LITERAL)
            (load-op! "addiu"  $4 $2 $6)]
           [(and REGISTER COMMA REGISTER COMMA REGISTER)
            (load-op! "and"  $4 $6 $2 0)]
           [(andi REGISTER COMMA REGISTER COMMA LITERAL)
            (load-op! "andi"  $4 $2 $6)]
           [(nor REGISTER COMMA REGISTER COMMA REGISTER)
            (load-op! "nor"  $4 $6 $2 0)]
           [(or REGISTER COMMA REGISTER COMMA REGISTER)
            (load-op! "or"  $4 $6 $2 0)]
           [(ori REGISTER COMMA REGISTER COMMA LITERAL)
            (load-op! "ori"  $4 $2 $6)]
           [(xor REGISTER COMMA REGISTER COMMA REGISTER)
            (load-op! "xor"  $4 $6 $2 0)]
           [(xori REGISTER COMMA REGISTER COMMA LITERAL)
            (load-op! "xori"  $4 $2 $6)]
           [(sll REGISTER COMMA REGISTER COMMA LITERAL)
            (load-op! "sll"  0 $4 $2 $6)]
           [(srl REGISTER COMMA REGISTER COMMA LITERAL)
            (load-op! "srl"  0 $4 $2 $6)]
           [(sra REGISTER COMMA REGISTER COMMA LITERAL)
            (load-op! "sra"  0 $4 $2 $6)]
           [(sllv REGISTER COMMA REGISTER COMMA REGISTER)
            (load-op! "sllv"  $2 $4 $6 0)]
           [(srlv REGISTER COMMA REGISTER COMMA REGISTER)
            (load-op! "srlv"  $2 $4 $6 0)]
           [(srav REGISTER COMMA REGISTER COMMA REGISTER)
            (load-op! "srav"  $2 $4 $6 0)]
           [(slt REGISTER COMMA REGISTER COMMA REGISTER)
            (load-op! "slt"  $4 $6 $2 0)]
           [(slti REGISTER COMMA REGISTER COMMA LITERAL)
            (load-op! "slti"  $4 $2 $6)]
           [(sltiu REGISTER COMMA REGISTER COMMA LITERAL)
            (load-op! "sltiu"  $4 $2 $6)]
           [(sltu REGISTER COMMA REGISTER COMMA REGISTER)
            (load-op! "sltu"  $4 $6 $2 0)]
           [(beq REGISTER COMMA REGISTER COMMA WORD)
            (defer-op! "beq"  $2 $4 `(branch ,$6))]
           [(bne REGISTER COMMA REGISTER COMMA WORD)
            (defer-op! "bne"  $2 $4 `(branch ,$6))]
           [(blt REGISTER COMMA REGISTER COMMA WORD) ; TODO TEST!!!
            (begin (load-op! "slt"  $2 $4 (which-register? "at") 0)
                   (defer-op! "bne"  (which-register? "at") (which-register? "zero") `(branch ,$6)))]
           [(bgt REGISTER COMMA REGISTER COMMA WORD) ; TODO TEST!!!
            (begin (load-op! "slt"  $4 $2 (which-register? "at") 0)
                   (defer-op! "bne"  (which-register? "at") (which-register? "zero") `(branch ,$6)))]
           [(ble REGISTER COMMA REGISTER COMMA WORD) ; TODO TEST!!!
            (begin (load-op! "slt"  $4 $2 (which-register? "at") 0)
                   (defer-op! "beq"  (which-register? "at") (which-register? "zero") `(branch ,$6)))]
           [(bge REGISTER COMMA REGISTER COMMA WORD) ; TODO TEST!!!
            (begin (load-op! "slt"  $2 $4 (which-register? "at") 0)
                   (defer-op! "beq"  (which-register? "at") (which-register? "zero") `(branch ,$6)))]
           [(j WORD)
            (defer-op! "j" `(jump ,$2))]
           [(jal WORD)
            (defer-op! "jal" `(jump ,$2))]
           [(jr REGISTER)
            (load-op! "jr" $2 0 0 0)]
           [(jalr REGISTER)
            (load-op! "jalr" $2 0 0 0)]
           [(move REGISTER COMMA REGISTER)
            (load-op! "add" 0 $4 $2 0)]
           [(syscall)
            (load-op! "syscall")]
           [(lb REGISTER COMMA LITERAL OP REGISTER CP)
            (load-op! "lb" $6 $2 $4)]
           [(lbu REGISTER COMMA LITERAL OP REGISTER CP)
            (load-op! "lbu" $6 $2 $4)]
           [(lh REGISTER COMMA LITERAL OP REGISTER CP)
            (load-op! "lh" $6 $2 $4)]
           [(lhu REGISTER COMMA LITERAL OP REGISTER CP)
            (load-op! "lhu" $6 $2 $4)]
           [(lui REGISTER COMMA LITERAL)
            (load-op! "lui" 0 $2 $4)]
           [(lw REGISTER COMMA LITERAL OP REGISTER CP)
            (load-op! "lw" $6 $2 $4)]
           [(li REGISTER COMMA LITERAL)
            (begin (load-op! "lui" 0 $2 (bitwise-bit-field $4 16 32))
                   (load-op! "ori" $2 $2 (bitwise-bit-field $4 0 16)))]
           [(la REGISTER COMMA WORD)
            (begin (defer-op! "lui" 0 $2 `(la-high ,$4))
                   (defer-op! "ori" $2 $2 `(la-low ,$4)))]
           [(sb REGISTER COMMA LITERAL OP REGISTER CP)
            (load-op! "sb" $6 $2 $4)]
           [(sw REGISTER COMMA LITERAL OP REGISTER CP)
            (load-op! "sw" $6 $2 $4)])))
   lexer)
  (when (> (length post-compile-operations) 0)
    (displayln "Second pass...")
    (map (λ (op) (op)) post-compile-operations)))

(define (asm-load-into-machine input)
  (port-count-lines! input)
  (define m (new mips-machine%))
  (asm-parse! m (λ () (asm-lex input)))
  (send m reset-pc-text!)
  m)

;;;
(define double-test (open-input-string #<<EOF

              # Successively double numbers
              li     $v0, 5                   # read int
              syscall
              move   $a0, $v0
notbigenough: add    $a0, $a0, $a0            # a0 *= 2 until a0 < 1250
              li     $t0, 1250
              li     $v0, 1                   # print
              syscall

              blt    $a0, $t0, notbigenough(

              li     $v0, 10                  # exit
              syscall

EOF
))

(define data-test (open-input-string #<<EOF

.data
a:   .asciiz "Hello there, enter your name> "
b:   .asciiz "Thank you, "
c:   .asciiz "!\n"
spc: .space 11
.align 2

.text
     li    $v0, 4
     la    $a0, a
     syscall

     la    $a0, spc
     li    $a1, 10
     li    $v0, 8
     syscall

     li    $v0, 4
     la    $a0, b
     syscall
     la    $a0, spc
     syscall
     la    $a0, c
     syscall

     li    $v0, 10
     syscall

EOF
))

(define m (asm-load-into-machine data-test))