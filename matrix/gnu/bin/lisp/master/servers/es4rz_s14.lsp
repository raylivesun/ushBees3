;; In hexadecimal and octal escape sequences, the escaped character code may contain a
;; variable number of digits, so the first subsequent character which is not a valid hexadecimal
;; or octal digit terminates the escape sequence. If the next character in a string could be
;; interpreted as a hexadecimal or octal digit, write ‘\ ’ (backslash and space) to terminate the
;; escape sequence. For example, ‘\xe0\ ’ represents one character, ‘a’ with grave accent. ‘\ ’
;; in a string constant is just like backslash-newline; it does not contribute any character to
;; the string, but it does terminate any preceding hex escape.


;; Hexadecimal escape sequences
(define-syntax string-hex-escape
  (syntax-rules (;; Hexadecimal escape sequences
   ;; \xHH escape sequence (2 hexadecimal digits)
   ;; \x{HHHHHHHH} escape sequence (8 hexadecimal digits)
   ;; \x{HHHH} escape sequence (4 hexadecimal digits)

    ;; \xHH escape sequence
    (hex-escape-sequence _ #xHH)
    (hex-escape-sequence _ #xH)
    (hex-escape-sequence _ #x)

    ;; \x{HHHHHHHH} escape sequence
    (hex-escape-sequence _ #x{HHHHHHHH})
    (hex-escape-sequence _ #x{HHHH})
    (hex-escape-sequence _ #x{HH})
    (hex-escape-sequence _ #x{H})
    (hex-escape-sequence _ #x{})

    ;; \x escape sequence
    (hex-escape-sequence _ #x)

    ;; \xHH escape sequence (with leading zero)
    (hex-escape-sequence _ #x0H)
    (hex-escape-sequence _ #x0)
    ;; ...
    ;; \x{HHHHHHHH} escape sequence (with leading zeros)
    (hex-escape-sequence _ #x{00000000})
    (hex-escape-sequence _ #x{000000})
    (hex-escape-sequence _ #x{0000})
    (hex-escape-sequence _ #x{00})
    (hex-escape-sequence _ #x{0})

    ;; Any other character
    (_ _ _)
    ))
    (let ((str (string-trim (syntax-object-text (syntax-e syntax)))))
     (string-replace-regexp (regexp "\\" (regexp-quote #x))
      (lambda (x)
        (case (string-length x)
         (2 (string-append "\\" (char (string->number (substring x 1
         2) 16))))
         (8 (string-append "\\" (char (string->number (substring x 1

         8) 16))))
         (4 (string-append "\\" (char (string->number (substring x 1
         4) 16))))

         ;; The rest of the characters are left as is
         (t x)))
         str)))))))

         ;; Octal escape sequences
         (define-syntax string-oct-escape
          (syntax-rules (;; Octal escape sequences
           ;; \oo escape sequence (3 octal digits)
           ;; \o{OOOOOOO} escape sequence (7 octal digits)
           ;; \o{OOO} escape sequence (3 octal digits)
           ;; \o{OO} escape sequence (2 octal digits)

           ;; \oo escape sequence
           (oct-escape-sequence _ #oOO)
           (oct-escape-sequence _ #oO)
           (oct-escape-sequence _ #o)
           ;; ...
           ;; \o{OOOOOOO} escape sequence
           (oct-escape-sequence _ #o{OOOOOOO})
           (oct-escape-sequence _ #o{OOOO})
           (oct-escape-sequence _ #o{OO})
           (oct-escape-sequence _ #o{O})
           (oct-escape-sequence _ #o{})

           ;; \o escape sequence
           (oct-escape-sequence _ #o)
           ;; \oo escape sequence (with leading zero)
           (oct-escape-sequence _ #o0O)
           (oct-escape-sequence _ #o0)
           ;; ...
           ;; \o{OOOOOOO} escape sequence (with leading zeros)
           (oct-escape-sequence _ #o{00000000})
           (oct-escape-sequence _ #o{000000})
           (oct-escape-sequence _ #o{0000})
           (oct-escape-sequence _ #o{00})
           (oct-escape-sequence _ #o{0})

           ;; Any other character
           (_ _ _)
           ))
           (let ((str (string-trim (syntax-object-text (syntax-e syntax)))))
            (string-replace-regexp (regexp "\\" (regexp-quote #o))
             (lambda (x)
               (case (string-length x)
                (3 (string-append "\\" (char (string->number (substring x 1
                3) 8))))
                (7 (string-append "\\" (char (string->number (substring x 1
                7) 8))))
                (3 (string-append "\\" (char (string->number (substring x 1
                3) 8))))
                (2 (string-append "\\" (char (string->number (substring x 1
                2) 8))))

                ;; The rest of the characters are left as is
                (t x)))
                str)))))))
                 ;; Unicode escape sequences
                 (define-syntax string-unicode-escape
                  (syntax-rules (;; Unicode escape sequences
                   ;; \uXXXX escape sequence (4 hexadecimal digits)
                   ;; \u{XXXXXXXX} escape sequence (8 hexadecimal digits)
                   ;; \u{XXXX} escape sequence (4 hexadecimal digits)
                   ;; \u{XX} escape sequence (2 hexadecimal digits)
                   ;; \u escape sequence (4 hexadecimal digits)

                   ;; \uXXXX escape sequence
                   (unicode-escape-sequence _ #uXXXX)
                   (unicode-escape-sequence _ #uXXX)
                   (unicode-escape-sequence _ #uXX)
                   (unicode-escape-sequence _ #uX)
                   ;; ...
                   ;; \u{XXXXXXXX} escape sequence



;; \u escape sequence
(unicode-escape-sequence _ #u)


